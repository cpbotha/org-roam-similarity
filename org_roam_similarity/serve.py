import asyncio
import logging
import os
import re
import time
from contextlib import asynccontextmanager
from pathlib import Path
from typing import List

import click
import pandas as pd
import uvicorn
from fastapi import FastAPI
from pydantic import BaseModel

from . import utils

logging.basicConfig(level=logging.INFO)

ctx = {"idle_time": 0}


def load_model(model_name):
    model, tokenizer = utils.get_model_and_tokenizer(model_name)

    if model.device.type == "cpu":
        logging.warning("⚠️  model is on CPU, this might be slow")
    else:
        logging.info(f"✅ model is on device {model.device}")

    return model, tokenizer


def load_embs(fn):
    embs_df_n = pd.read_parquet(fn)
    # once-off normalize the embeddings if they are not pre-normalized
    # embs_df_n = embs_df.div(np.linalg.norm(embs_df, axis=1), axis=0)
    # strip out the sha1 column, we only want hthe embeddings
    return embs_df_n.drop("sha1", axis=1)


async def bleh():
    sleep_increment = 5

    while True:
        await asyncio.sleep(sleep_increment)
        ctx["idle_time"] += sleep_increment
        if ctx["model"] is not None and ctx["idle_time"] > 600:
            logging.info("💤 Idle for too long, shutting down model, will reactivate on new requests.")
            # remove ctx["model"] from the GPU
            ctx["model"] = None
            ctx["tokenizer"] = None
            import gc

            gc.collect()


@asynccontextmanager
async def lifespan(app: FastAPI):
    ctx["max_length"] = int(os.environ.get("ORS_MAX_LENGTH"))
    logging.info(f"max_length={ctx['max_length']}")
    fn = os.environ.get("ORS_EMBEDDINGS_FN")
    ctx["embs_df_n"] = load_embs(fn)

    # path should be: ors_embs.MODEL_NAME.parq
    # if it's not, this code will break
    ctx["model_name"] = re.match(r"ors_embs\.(.+)\.parq", Path(fn).name).group(1).replace("---", "/")
    # we're going to load the model on-demand
    ctx["model"], ctx["tokenizer"] = None, None

    # start async task that sleeps most of the time, but checks idle time
    # periodically and shuts down if idle for too long
    sleeper_task = asyncio.create_task(bleh())

    yield

    # Clean up the ML models and release the resources
    sleeper_task.cancel()


app = FastAPI(lifespan=lifespan)


class TextObject(BaseModel):
    text: str


@app.post("/similar/")
def get_similar_nodes(text_object: TextObject) -> List:
    ctx["idle_time"] = 0
    if ctx["model"] is None:
        ctx["model"], ctx["tokenizer"] = load_model(ctx["model_name"])

    # time the embedding in microseconds
    start_ns = time.perf_counter_ns()
    # emb is a numby array. 512 elements in the case of jina v2 small en
    emb_ = utils.encode(text_object.text, ctx["tokenizer"], ctx["model"])
    # emb = ctx["model"].encode(text_object.text, max_length=ctx["max_length"])
    emb = emb_[0].cpu().numpy()

    end_emb_ns = time.perf_counter_ns()
    # series with index the ID and value the similarity
    top_n = ctx["embs_df_n"].dot(emb).sort_values(ascending=False).head(10)
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
