#!/bin/bash
set -e

# Image name
IMAGE_NAME="tokac-builder:latest"
BUILD_DIR="build-linux"
BINARY_NAME="tokac-linux"

echo "=== 1. Preparing Builder Image ==="
# Docker will cache this step. It only rebuilds if Dockerfile changes.
# First run will be slow; subsequent runs are instant.
docker build -t $IMAGE_NAME -f docker/linux/Dockerfile .

echo "=== 2. Compiling for Linux (inside container) ==="
# We mount current dir to /app, build in build-linux, and output binary
docker run --rm -v "$(pwd):/app" $IMAGE_NAME /bin/bash -c "
    mkdir -p $BUILD_DIR && cd $BUILD_DIR && \
    cmake .. -DCMAKE_BUILD_TYPE=Release && \
    make -j$(nproc) && \
    mkdir -p ../bin && \
    cp src/tokac ../bin/$BINARY_NAME
"

echo "=== 3. Build Complete ==="
echo "Linux binary available at: bin/$BINARY_NAME"
ls -lh bin/$BINARY_NAME
file $BINARY_NAME
