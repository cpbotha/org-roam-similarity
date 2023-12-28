from pathlib import Path

import torch
import torch.nn.functional as F
from transformers import AutoModel, AutoTokenizer

MODEL_LUT = ["sentence-transformers/multi-qa-MiniLM-L6-cos-v1", "jinaai/jina-embeddings-v2-small-en"]


# - nreimers/MiniLM-L6-H384-uncased == multi-qa-MiniLM-L6-cos-v1
#   max context length 512
# - jinaai/jina-embeddings-v2-small-en
def get_model_and_tokenizer(model_name="jinaai/jina-embeddings-v2-small-en"):
    try:
        model = AutoModel.from_pretrained(model_name, trust_remote_code=True, device_map="auto")
    except ValueError as e:
        print(f"ValueError: {e}")
        print("Model possibly does not support device_map='auto', trying more direct fallback...")
        device = torch.device(
            "cuda" if torch.cuda.is_available() else "mps" if torch.backends.mps.is_available() else "cpu"
        )
        print(f"Found device: {device}")
        model = AutoModel.from_pretrained(model_name, trust_remote_code=True)
        model.to(device)

    tokenizer = AutoTokenizer.from_pretrained(model_name)

    return model, tokenizer


# Mean Pooling - Take average of all tokens
def mean_pooling(model_output, attention_mask):
    # should be the same as model_output[0] for transformers
    token_embeddings = model_output.last_hidden_state
    input_mask_expanded = attention_mask.unsqueeze(-1).expand(token_embeddings.size()).float()
    return torch.sum(token_embeddings * input_mask_expanded, 1) / torch.clamp(input_mask_expanded.sum(1), min=1e-9)


def encode(texts, tokenizer, model):
    # Tokenize sentences
    encoded_input = tokenizer(texts, padding=True, truncation=True, return_tensors="pt").to(model.device)

    # Compute token embeddings
    with torch.no_grad():
        model_output = model(**encoded_input, return_dict=True)

    # Perform pooling
    embeddings = mean_pooling(model_output, encoded_input["attention_mask"])

    # Normalize embeddings
    embeddings = F.normalize(embeddings, p=2, dim=1)

    return embeddings


def get_embs_path(dir, model_name):
    slug = model_name.replace("/", "---")
    return dir / f"ors_embs.{slug}.parq"
