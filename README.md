# org-roam-similarity

Show similar org-roam-nodes, similarity determined by embeddings, to the node you are currently working on.

<a href="http://www.youtube.com/watch?feature=player_embedded&v=cHQx4ITQRNU
" target="_blank"><img src="http://img.youtube.com/vi/cHQx4ITQRNU/0.jpg" 
alt="Demo of org-roam-similarity" width="240" height="180" border="10" /></a>

What you're seeing in the demo above, is org-roam node (subtree or file) live Jina AI (fully local) similarity search in the org-roam buffer, along with your backlinks and reflinks. This automatically surfaces other org-roam nodes which are related to the one you're currently reading, or even working on!

This open source setup currently works as follows:

- export all of your org-roam nodes as text files using supplied emacs-lisp
- use embed.py to calculate embeddings for all of these txt files and store them in a parq file
- run serve.py which waits for submission of any text to return the N closest node ids, according to the Jina AI learned embeddings. These are really quite good and fully local, but it would be straight-forward to use a service like OpenAI embeddings for everything
- There is more emacs lisp that customizes the org-roam buffer setup to call to serve.py's endpoint and renders the list of similar nodes

## Quickstart

TBD

## Setup your dev environment

Setup Python virtual environment and install dependencies:

```shell
poetry env use 3.11
poetry install -vv
```

Configure nbdime git drivers for notebooks:

```shell
nbdime config-git --enable
```

### A note on nvidia dependencies for the hf transformers package

Using this via transformers required the whole of CUDA, cudnn and at the end also libnccl. It's a whole lot easier just using simon willison's llm cli!

(WHY IS NCCL SO NASTY TO INSTALL?!)

On WSL with Ubuntu 22.04, I upgraded to CUDA 12.1 following https://developer.nvidia.com/cuda-12-1-0-download-archive?target_os=Linux&target_arch=x86_64&Distribution=WSL-Ubuntu&target_version=2.0&target_type=deb_network

I could also have stuck with the CUDA 11.8 packages of PyTorch 2.1, but it looks like 12.1 is now the default
