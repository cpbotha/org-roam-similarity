import logging
import os
import time
from contextlib import asynccontextmanager
from typing import List

import click
import numpy as np
import pandas as pd
import uvicorn
from fastapi import FastAPI
from pydantic import BaseModel

from . import utils

logging.basicConfig(level=logging.INFO)

ctx = {}


def load_model():
    model = utils.get_model()

    if model.device.type == "cpu":
        logging.warning("⚠️ model is on CPU, this will be slow")
    else:
        logging.info(f"✅ model is on device {model.device}")

    return model


def load_embs(fn):
    embs_df_n = pd.read_parquet(fn)
    # once-off normalize the embeddings if they are not pre-normalized
    # embs_df_n = embs_df.div(np.linalg.norm(embs_df, axis=1), axis=0)
    # strip out the sha1 column, we only want hthe embeddings
    return embs_df_n.drop("sha1", axis=1)


@asynccontextmanager
async def lifespan(app: FastAPI):
    ctx["max_length"] = int(os.environ.get("ORS_MAX_LENGTH"))
    logging.info(f"max_length={ctx['max_length']}")
    ctx["embs_df_n"] = load_embs(os.environ.get("ORS_EMBEDDINGS_FN"))
    ctx["model"] = load_model()
    yield
    # Clean up the ML models and release the resources
    pass


app = FastAPI(lifespan=lifespan)


class TextObject(BaseModel):
    text: str


@app.post("/similar/")
def get_similar_nodes(text_object: TextObject) -> List:
    # time the embedding in microseconds
    start_ns = time.perf_counter_ns()
    # emb is a numby array. 512 elements in the case of jina v2 small en
    emb = ctx["model"].encode(text_object.text, max_length=ctx["max_length"])
    emb_n = emb / np.linalg.norm(emb)
    end_emb_ns = time.perf_counter_ns()
    # series with index the ID and value the similarity
    top_n = ctx["embs_df_n"].dot(emb_n).sort_values(ascending=False).head(10)
    end_ns = time.perf_counter_ns()
    logging.info(
        f"Embedding {len(text_object.text)} bytes ({(end_emb_ns-start_ns)/1e06} ms), brute force similarity and sorting: {(end_ns - end_emb_ns) / 1000_000} ms"
    )
    # logging.warning(text)
    logging.info(top_n.values)
    return zip(top_n.index.to_list(), top_n.values.tolist())


@click.command()
@click.argument("embeddings_fn")
# NOTE: leaving this at None (the default) with long files (e.g. 210KB) would
#       result in Jina increasing ALIBI size to 64K+ from 8192 and OOMing my GPU
#       so now rather defaulting to 8192, max for Jina v2
@click.option(
    "--max_length",
    type=int,
    default=8192,
    required=False,
    help="Optionally decrease context length to speed up embedding, at the cost of document truncation.",
)
@click.option("--reload", is_flag=True)
def run(embeddings_fn, max_length, reload):
    """Run orserve for production."""

    # we have to do this strange dance sending CLI argumnents via the
    # environment to be picked up by the lifespan function so that reloading
    # works without issues (run() function is only executed first time, it then
    # invokes uvicorn which imports this file)
    os.environ["ORS_MAX_LENGTH"] = str(max_length)
    os.environ["ORS_EMBEDDINGS_FN"] = embeddings_fn

    uvicorn.run("org_roam_similarity.serve:app", port=3814, reload=reload)


if __name__ == "__main__":
    run()
