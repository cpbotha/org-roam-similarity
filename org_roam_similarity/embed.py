from pathlib import Path


import numpy as np
import pandas as pd
import click
from tqdm import tqdm
from transformers import AutoModel

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
@click.argument("glob")
def cli(proj: str, path: Path, glob: str):

    model_name = "jinaai/jina-embeddings-v2-small-en"
    model = AutoModel.from_pretrained(
        model_name, trust_remote_code=True, device_map="auto"
    )  # trust_remote_code is needed to use the encode method

    if model.device.type == "cpu":
        click.echo("⚠️ model is on CPU, this will be slow")
    else:
        click.echo(f"✅ model is on device {model.device}")


    target_dir = Path(path)

    embs = {}
    for f in tqdm(list(Path(path).glob(glob))):
        txt = f.open().read()

        emb = model.encode(txt)
        # f.stem by convention is the ID
        # also, we pre-normalize the embeddings so that we don't have to do it at query time
        embs[f.stem] = emb / np.linalg.norm(emb)

    # dict key should be row index, hence index
    df = pd.DataFrame.from_dict(embs, orient="index")
    df.to_parquet(target_dir / f"{proj}.norm_embs.parq")

    click.echo()

if __name__ == "__main__":
    cli()