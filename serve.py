import logging
from typing import Dict, List

from fastapi import FastAPI, Response
import numpy as np
import pandas as pd
import uvicorn

from transformers import AutoModel

device = "cuda:0"
# vs base
model_name = "jinaai/jina-embeddings-v2-small-en"
model = AutoModel.from_pretrained(
    model_name, trust_remote_code=True
)  # trust_remote_code is needed to use the encode method
model.to(device)


embs_df_n = pd.read_parquet("/tmp/bleh2/bleh2.norm_embs.parq")
# once-off normalize the embeddings if they are not pre-normalized
#embs_df_n = embs_df.div(np.linalg.norm(embs_df, axis=1), axis=0)

app = FastAPI()


@app.get("/similar/")
def get_similar_nodes(text: str) -> List:
    # emb is a numby array. 512 elements in the case of jina v2 small en
    emb = model.encode(text)
    emb_n = emb / np.linalg.norm(emb)
    # series with index the ID and value the similarity
    top_n = embs_df_n.dot(emb_n).sort_values(ascending=False).head(10)
    #logging.warning(text)
    return zip(top_n.index.to_list(), top_n.values.tolist())


def run():
    """Run orserve for production."""
    uvicorn.run(app)


def run_dev():
    """Run orserve with auto-reload for development."""
    uvicorn.run("org_roam_canvas.orserve:app", reload=True)


if __name__ == "__main__":
    run()
