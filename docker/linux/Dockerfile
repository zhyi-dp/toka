# Use Ubuntu 22.04 (Jammy) as base
FROM ubuntu:22.04

# Avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies for adding repositories
RUN apt-get update && apt-get install -y \
    wget \
    lsb-release \
    software-properties-common \
    gnupg \
    && rm -rf /var/lib/apt/lists/*

# Install LLVM 17 using the official script
RUN wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 17 all

# Install other build tools matching dev environment
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    zlib1g-dev \
    libzstd-dev \
    && rm -rf /var/lib/apt/lists/*

# Set LLVM 17 as default
ENV LLVM_DIR=/usr/lib/llvm-17/lib/cmake/llvm
ENV CC=clang-17
ENV CXX=clang++-17

# Create workspace
WORKDIR /app
