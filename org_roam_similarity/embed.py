import hashlib
from pathlib import Path

import click
import numpy as np
import pandas as pd
from tqdm import tqdm

from . import utils


@click.command()
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
@click.option("--model", type=int, default=0, required=False, help="0: MiniLM, 1: jina-embeddings-v2-small-en")
@click.option(
    "--max_length",
    type=int,
    default=None,
    required=False,
    help="Optionally decrease context length to speed up embedding, at the cost of document truncation.",
)
def cli(path: Path, glob: str, model=0, max_length=None):
    model_name = utils.MODEL_LUT[model]
    model, tokenizer = utils.get_model_and_tokenizer(model_name)

    if model.device.type == "cpu":
        click.echo("⚠️ model is on CPU, this will be slow")
    else:
        click.echo(f"✅ model is on device {model.device}")

    target_dir = Path(path)

    embs_path = utils.get_embs_path(target_dir, model_name)
    if embs_path.exists():
        click.echo("✅ loading previous embeddings for caching")
        prev_df = pd.read_parquet(embs_path)
        # print(prev_df.head())
        # prev_sha_df has the same index, but maps to sha1
        prev_sha1_df = prev_df["sha1"]
        prev_embs_df = prev_df.drop("sha1", axis=1)

    else:
        prev_df = None

    embs = {}
    shas = {}
    cache_hits = 0
    for f in tqdm(list(Path(path).glob(glob))):
        try:
            txt = f.open(encoding="utf-8").read()
        except UnicodeDecodeError as e:
            print(repr(e))
            click.echo(f"⚠️ Unicode error reading {f}, skipping...")
            continue

        # e.g. 1da13263b16314112f6886f6c81ecc133289d52f
        sha1 = hashlib.sha1(txt.encode()).hexdigest()
        id = f.stem
        # read out previous sha1 if id exists
        if prev_df is not None and id in prev_df.index:
            prev_sha1 = prev_sha1_df.loc[id]
            if sha1 == prev_sha1:
                # if the sha1 hasn't changed, we can skip this file
                # the previous embedding comes out as pd.Series, let's cast to numpy array
                embs[id] = prev_embs_df.loc[id].to_numpy()
                shas[id] = sha1
                cache_hits += 1
                continue

        # you can use max_length=4096 or even less to speed up and use less ram
        # generally we try to do max context 8192 for initial embedding, but smaller context for inference
        emb = utils.encode(txt, tokenizer, model)  # , max_length=max_length)
        # f.stem by convention is the ID
        # we gave it a single text, so we get out a batch of size 1, hence the emb[0]
        # then move to cpu, and convert to numpy
        embs[id] = emb[0].cpu().numpy()
        shas[id] = sha1

    # dict key should be row index, hence index
    df = pd.DataFrame.from_dict(embs, orient="index")
    df["sha1"] = pd.Series(shas)
    df.to_parquet(embs_path)
    click.echo(f"Wrote {len(df)} embeddings to {embs_path} ({cache_hits} cache hits).")

    click.echo()


if __name__ == "__main__":
    cli()
