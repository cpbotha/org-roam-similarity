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

The steps, explained in more detail in the following subsections, are as follows:

1. install org-roam-similarity.el
2. export nodes and create embeddings (Python)
3. start server (Python)
4. use the familiar org-roam buffer

### Install org-roam-similarity Emacs support

Add the included [org-roam-similarity](./org-roam-similarity.el) to your Emacs init.

There are various ways to do this, e.g. with [straightel](https://github.com/radian-software/straight.el). On Emacs 29.1 or later, you can use the built-in `package-vc-install` function:

```emacs-lisp
(unless (package-installed-p 'org-roam-similarity)
 (package-vc-install "https://github.com/cpbotha/org-roam-similarity"))
```

### Configure the org-roam buffer to use similarity

Add the following to your Emacs init:

```emacs-lisp
(setq org-roam-mode-sections (list #'org-roam-similarity-section
                                   #'org-roam-backlinks-section
                                   #'org-roam-reflinks-section))
```

Here we add the similarity section to the existing two backlinks and reflinks sections.

### Key bind the ad-hoc similarity search

With this interactive function, you can find the org-roam nodes that are similar to the current region (if active) or the whole buffer, no matter whether it's a node or not.

``` emacs-lisp
(global-set-key (kbd "C-c n S") 'org-roam-node-find-similar)
```

### Install the org-roam-similarity Python code

There are at least three ways to get this running.

#### pipx

The easiest way to do this is if you have [pipx](https://github.com/pypa/pipx) installed:

```shell
pipx install git+https://github.com/cpbotha/org-roam-similarity
```

You can now invoke either `embed_ors` or `serve_ors`.

#### uvx / uv tool

Use `uv tool` to install the package in a self-contained virtualenv, just like `pipx`. You'll be able to invoke `server_ors` and `embed_ors` from the `~/.local/bin` directory.

```shell
# at the time of writing (2024-10-07) pyarrow only supports up to python 3.11
# uvx (uv 0.4.18) does not honour the python package version limitation in pyproject, so here we instruct it
uv tool install --python `which python3.11` git+https://github.com/cpbotha/org-roam-similarity
```

See [this PR description](https://github.com/astral-sh/uv/issues/3560) for more details about tools.

#### git clone the repo, then poetry run

Alternatively, you can also opt to ~git clone~ the repo, and then do the following in the checked-out directory:

```shell
poetry install
```

In this case, you can invoke the e.g. the ors server with `poetry run serve_ors`

### Export and embed your org-roam database

Create a temporary directory, e.g. `/tmp/ors/`.

With `org-roam-similarity.el` installed, invoke `M-x ors-export-org-roam-nodes` and enter `/tmp/ors` when asked.

This will export all of your org-roam nodes into .txt files, named according to the org-roam ID.

Now create the embeddings by doing:

```shell
# or poetry run embed_ors ... if you chose poetry
# --model 0: sentence-transformers/multi-qa-MiniLM-L6-cos-v1 -- smaller and faster
# --model 1: jinaai/jina-embeddings-v2-small-en -- larger and more accurate
embed_ors /tmp/ors/
```

On my RTX 2070 GPU, this Jina a minute or two for 1700 nodes. On my M1 Pro with the mps (Metal) backend, it takes 10 to 15 minutes. MiniLM (model 0) takes about 5 minutes on the M1 Pro, so 3x faster.

This will create a file named `/tmp/ors/<MODEL_NAME>.norm_embs.parq` which contains the 512-dimensional (384 for MiniLM) embeddings for all of your nodes.

If you keep that parq file there, the next time you embed, it will use a hash-based caching mechanism to embed only the nodes that you added.

You can also copy the parq file around and use it anywhere else. The server only requires the parq file and not the exported txt files.

### Start the server

```shell
serve_ors /tmp/ors/<MODEL_NAME>.norm_embs.parq
```

### Try it out

With an org-roam node showing, activate the org-roam-buffer with `M-x org-roam-buffer-toggle` or `C-c n l`.

In the server logs you should see it being hit, and then the similar nodes will appear in the familiar `org-roam-buffer`. At every save or org-roam navigate, the buffer will update with the new list of most similar nodes.

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

On WSL with Ubuntu 22.04, I upgraded to CUDA 12.1 following <https://developer.nvidia.com/cuda-12-1-0-download-archive?target_os=Linux&target_arch=x86_64&Distribution=WSL-Ubuntu&target_version=2.0&target_type=deb_network>

I could also have stuck with the CUDA 11.8 packages of PyTorch 2.1, but it looks like 12.1 is now the default

## Related work

[Semantic Search for Org roam](https://lgmoneda.github.io/2023/04/08/semantic-search-for-org-roam.html) by Luis Moneda is a great blog post and accompanying implementation for a fairly similar solution to this one.

In addition to me only discovering Luis's work after publishing this package, it differs from Luis's work in the following ways: It uses exclusively local models for the document embedding, it uses the built-in org-roam functions for iterating over all nodes, it integrated with the existing `org-roam-buffer`, and it pretends to be configurable by others in a reasonable time. Based on his blog post, Luis wanted to experiment some more with the prototype system that he had built.

[org-similarity](https://github.com/brunoarine/org-similarity) also does similarity searches, but it uses
older, slightly less accurate but much faster lexical similarity calculations like TF-IDF and BM25.
