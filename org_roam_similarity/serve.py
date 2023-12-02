import logging
import time
from typing import Dict, List


import click
from fastapi import FastAPI, Response
import numpy as np
import pandas as pd
from pydantic import BaseModel
import uvicorn

from . import utils


logging.basicConfig(level=logging.INFO)

MAX_LENGTH = None
EMBS_DF_N = None
MODEL = None

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
    #embs_df_n = embs_df.div(np.linalg.norm(embs_df, axis=1), axis=0)
    # strip out the sha1 column, we only want hthe embeddings
    return embs_df_n.drop("sha1", axis=1)

app = FastAPI()

class TextObject(BaseModel):
    text: str

@app.post("/similar/")
def get_similar_nodes(text_object: TextObject) -> List:
    # time the embedding in microseconds
    start_ns = time.perf_counter_ns() 
    # emb is a numby array. 512 elements in the case of jina v2 small en
    emb = MODEL.encode(text_object.text, max_length=MAX_LENGTH)
    emb_n = emb / np.linalg.norm(emb)
    # series with index the ID and value the similarity
    top_n = EMBS_DF_N.dot(emb_n).sort_values(ascending=False).head(10)
    end_ns = time.perf_counter_ns()
    logging.info(f"Embedding {len(text_object.text)}, brute force similarity and sorting took {(end_ns - start_ns) / 1000_000} ms")
    #logging.warning(text)
    logging.info(top_n.values)
    return zip(top_n.index.to_list(), top_n.values.tolist())

@click.command()
@click.argument("embeddings_fn")
@click.option("--max_length", type=int, default=None, required=False, 
              help="Optionally decrease context length to speed up embedding, at the cost of document truncation.")
@click.option("--reload", is_flag=True)
def run(embeddings_fn, max_length, reload):
    """Run orserve for production."""
    global EMBS_DF_N, MAX_LENGTH, MODEL
    MAX_LENGTH = max_length

    EMBS_DF_N = load_embs(embeddings_fn)
    MODEL = load_model()

    uvicorn.run("org_roam_similarity.serve:app", port=3814, reload=reload)


if __name__ == "__main__":
    run()
