import hashlib
from pathlib import Path


import numpy as np
import pandas as pd
import click
from tqdm import tqdm

from . import utils

@click.command()
@click.argument("proj")
@click.argument(
    "path",
    type=click.Path(
        exists=True,
        file_okay=False,
        readable=True,
        path_type=Path,
    ),
)
# make default '*.txt'
@click.argument("glob", default="*.txt")
@click.option("--max_length", type=int, default=None, required=False, 
              help="Optionally decrease context length to speed up embedding, at the cost of document truncation.")
def cli(proj: str, path: Path, glob: str, max_length=None):

    model = utils.get_model()

    if model.device.type == "cpu":
        click.echo("⚠️ model is on CPU, this will be slow")
    else:
        click.echo(f"✅ model is on device {model.device}")

    target_dir = Path(path)

    embs_path = utils.get_embs_path(target_dir, proj) 
    if embs_path.exists():
        click.echo("✅ loading previous embeddings for caching")
        prev_df = pd.read_parquet(embs_path)
        print(prev_df.head())
        # prev_sha_df has the same index, but maps to sha1 
        prev_sha1_df = prev_df["sha1"]
        prev_embs_df = prev_df.drop("sha1", axis=1)

    else:
        prev_df = None

    embs = {}
    shas = {}
    cache_hits = 0
    for f in tqdm(list(Path(path).glob(glob))):
        txt = f.open().read()

        # e.g. 1da13263b16314112f6886f6c81ecc133289d52f
        sha1 = hashlib.sha1(txt.encode()).hexdigest()
        id = f.stem
        # read out previous sha1 if id exists
        if prev_df is not None and id in prev_df.index:
            prev_sha1 = prev_sha1_df.loc[id]
            if sha1 == prev_sha1:
                # if the sha1 hasn't changed, we can skip this file
                embs[id] = prev_embs_df.loc[id]
                shas[id] = sha1
                cache_hits += 1
                continue

        # you can use max_length=4096 or even less to speed up and use less ram
        # generally we try to do max context 8192 for initial embedding, but smaller context for inference
        emb = model.encode(txt)
        # f.stem by convention is the ID
        # also, we pre-normalize the embeddings so that we don't have to do it at query time
        embs[id] = emb / np.linalg.norm(emb)
        shas[id] = sha1

    # dict key should be row index, hence index
    df = pd.DataFrame.from_dict(embs, orient="index")
    df["sha1"] = pd.Series(shas)
    df.to_parquet(embs_path)
    click.echo(f"Wrote {len(df)} embeddings to {embs_path} ({cache_hits} cache hits).")

    click.echo()

if __name__ == "__main__":
    cli()