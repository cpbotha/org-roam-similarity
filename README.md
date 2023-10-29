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
