# LAPACK AI Modernization - Containerized Development Environment

## Overview

This document provides a complete guide for setting up a containerized development environment for the LAPACK AI Modernization project. The environment supports GPU-accelerated linear algebra operations, Python integration, and modern development practices through Docker containers.

## Environment Architecture

```
LAPACK AI Containerized Development
├── Host System (macOS/Linux/Windows)
│   ├── Docker Engine (OrbStack/Docker Desktop)
│   ├── GPU Runtime (NVIDIA Docker/OpenCL)
│   └── Container Orchestration
├── Development Container
│   ├── Build Tools (CMake, GCC, gfortran)
│   ├── Scientific Computing (NumPy, SciPy)
│   ├── GPU Computing (PyOpenCL)
│   ├── Web Framework (Flask, Jupyter)
│   └── Development Tools (pytest, black, mypy)
└── Production Container
    ├── Minimal Runtime
    ├── Performance Optimized
    └── Cloud Ready
```

## Prerequisites

### System Requirements

**Hardware:**
- x86_64 or ARM64 processor
- 8+ GB RAM (16+ GB recommended for development)
- GPU with OpenCL 1.2+ support (optional but recommended)
- 10+ GB free disk space (for containers and cache)

**Software:**
- Docker Engine 20.10+ or OrbStack (macOS)
- Docker Compose 2.0+
- NVIDIA Container Toolkit (for NVIDIA GPU support)

## Installation Guide

### Step 1: Install Docker

#### macOS (OrbStack - Recommended)

```bash
# Install OrbStack (faster, more efficient than Docker Desktop)
brew install orbstack

# Start OrbStack
orb

# Verify Docker is working
docker --version
docker run hello-world
```

#### macOS (Docker Desktop Alternative)

```bash
# Install Docker Desktop
brew install --cask docker

# Start Docker Desktop and verify
docker --version
```

#### Linux (Ubuntu/Debian)

```bash
# Install Docker Engine
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Add user to docker group
sudo usermod -aG docker $USER
newgrp docker

# Install Docker Compose
sudo apt install docker-compose-plugin

# Verify installation
docker --version
docker compose version
```

### Step 2: Install GPU Support (Optional)

#### NVIDIA GPU Support

```bash
# Linux: Install NVIDIA Container Toolkit
distribution=$(. /etc/os-release;echo $ID$VERSION_ID) \
   && curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add - \
   && curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list

sudo apt-get update
sudo apt-get install -y nvidia-docker2
sudo systemctl restart docker

# Test GPU access
docker run --rm --gpus all nvidia/cuda:11.8-runtime-ubuntu20.04 nvidia-smi
```

#### macOS/Windows GPU Support

```bash
# OpenCL support is built into the containers
# No additional installation required
```

### Step 3: Build Development Environment

```bash
# Clone/navigate to project directory
cd /path/to/lapack_ai

# Build base container
docker build -f MODERNIZATION/_docs/Dockerfile.base -t lapack-ai-base:latest .

# Build development container
docker build -f MODERNIZATION/dev_environment/Dockerfile.dev -t lapack-ai-dev:latest .

# Verify build
docker images | grep lapack-ai
```

### Step 4: Start Development Environment

```bash
# Quick start - interactive development
docker run -it --rm \
  --name lapack-dev \
  -v $(pwd):/opt/lapack-ai \
  -p 8888:8888 \
  -p 5000:5000 \
  --gpus all \
  lapack-ai-dev:latest

# Or start specific services
docker run -d --name lapack-jupyter \
  -v $(pwd):/opt/lapack-ai \
  -p 8888:8888 \
  --gpus all \
  lapack-ai-dev:latest jupyter
```

## Development Workflow

### 1. Container Management

```bash
# Start development environment
docker run -it --rm \
  --name lapack-dev \
  -v $(pwd):/opt/lapack-ai \
  -p 8888:8888 \
  -p 5000:5000 \
  -p 5001:5001 \
  --gpus all \
  lapack-ai-dev:latest

# Available commands in container:
# jupyter  - Start Jupyter Lab (access: http://localhost:8888)
# flask    - Start Flask development server (access: http://localhost:5000)
# test     - Run test suite
# build    - Build LAPACK with custom features
```

### 2. Development Services

#### Jupyter Lab Development
```bash
# Start Jupyter Lab
docker run -d --name lapack-jupyter \
  -v $(pwd):/opt/lapack-ai \
  -p 8888:8888 \
  --gpus all \
  lapack-ai-dev:latest jupyter

# Access at: http://localhost:8888
# No password required in development mode
```

#### Flask Development Server
```bash
# Start Flask development server
docker run -d --name lapack-flask \
  -v $(pwd):/opt/lapack-ai \
  -p 5000:5000 \
  --gpus all \
  lapack-ai-dev:latest flask

# Access at: http://localhost:5000
# Hot-reload enabled for development
```

#### Interactive Development Shell
```bash
# Get shell access to running container
docker exec -it lapack-dev bash

# Or start new container with shell
docker run -it --rm \
  -v $(pwd):/opt/lapack-ai \
  --gpus all \
  lapack-ai-dev:latest bash
```

### 3. Build and Test Commands

```bash
# Run tests in container
docker run --rm \
  -v $(pwd):/opt/lapack-ai \
  --gpus all \
  lapack-ai-dev:latest test

# Build LAPACK with custom features
docker run --rm \
  -v $(pwd):/opt/lapack-ai \
  --gpus all \
  lapack-ai-dev:latest build

# Format code
docker run --rm \
  -v $(pwd):/opt/lapack-ai \
  lapack-ai-dev:latest python -m black src/

# Type checking
docker run --rm \
  -v $(pwd):/opt/lapack-ai \
  lapack-ai-dev:latest python -m mypy src/
```

## Docker Compose for Development

Create `docker-compose.dev.yml` for easier management:

```yaml
version: '3.8'

services:
  dev:
    build:
      context: .
      dockerfile: MODERNIZATION/dev_environment/Dockerfile.dev
    volumes:
      - .:/opt/lapack-ai
    ports:
      - "8888:8888"
      - "5000:5000"
      - "5001:5001"
    stdin_open: true
    tty: true
    deploy:
      resources:
        reservations:
          devices:
            - driver: nvidia
              count: 1
              capabilities: [gpu]
    environment:
      - FLASK_ENV=development
      - FLASK_DEBUG=1

  jupyter:
    extends: dev
    command: jupyter
    ports:
      - "8888:8888"

  flask:
    extends: dev
    command: flask
    ports:
      - "5000:5000"
```

Usage:
```bash
# Start all services
docker compose -f docker-compose.dev.yml up -d

# Start specific service
docker compose -f docker-compose.dev.yml up jupyter

# Scale services
docker compose -f docker-compose.dev.yml up --scale flask=2

# View logs
docker compose -f docker-compose.dev.yml logs -f

# Stop all services
docker compose -f docker-compose.dev.yml down
```

## GPU Development

### Testing GPU Access

```bash
# Test OpenCL availability in container
docker run --rm --gpus all lapack-ai-dev:latest python -c "
import pyopencl as cl
platforms = cl.get_platforms()
print(f'Found {len(platforms)} OpenCL platforms')
for platform in platforms:
    devices = platform.get_devices()
    print(f'Platform: {platform.name}, Devices: {len(devices)}')
    for device in devices:
        print(f'  Device: {device.name}')
"

# Check GPU memory and capabilities
docker run --rm --gpus all lapack-ai-dev:latest python -c "
import pyopencl as cl
ctx = cl.create_some_context()
for device in ctx.devices:
    print(f'Device: {device.name}')
    print(f'Global Memory: {device.global_mem_size // 1024**3} GB')
    print(f'Compute Units: {device.max_compute_units}')
    print(f'Max Work Group Size: {device.max_work_group_size}')
"
```

### GPU Debugging

```bash
# Enable detailed GPU debugging
docker run -it --rm \
  -v $(pwd):/opt/lapack-ai \
  --gpus all \
  -e OCL_ENABLE_DEBUG=1 \
  -e OCL_DEBUG_LEVEL=3 \
  -e PYOPENCL_COMPILER_OUTPUT=1 \
  lapack-ai-dev:latest bash
```

## Performance Optimization

### Container Resource Limits

```bash
# Run with specific resource limits
docker run -it --rm \
  --memory=4g \
  --cpus=2 \
  --shm-size=1g \
  -v $(pwd):/opt/lapack-ai \
  --gpus all \
  lapack-ai-dev:latest
```

### Build Cache Optimization

```bash
# Use BuildKit for faster builds
export DOCKER_BUILDKIT=1

# Build with cache mount
docker build \
  --mount=type=cache,target=/var/cache/apt \
  --mount=type=cache,target=/root/.cache/pip \
  -f MODERNIZATION/_docs/Dockerfile.base \
  -t lapack-ai-base:latest .
```

## Production Container

### Build Production Image

```bash
# Build optimized production image
docker build -f MODERNIZATION/_docs/Dockerfile.prod -t lapack-ai-prod:latest .

# Check image size (should be <500MB)
docker images lapack-ai-prod:latest

# Test production container
docker run --rm lapack-ai-prod:latest health
```

### Production Deployment

```bash
# Run production web server
docker run -d \
  --name lapack-ai-web \
  -p 5000:5000 \
  --restart unless-stopped \
  --gpus all \
  lapack-ai-prod:latest web

# Run background worker
docker run -d \
  --name lapack-ai-worker \
  --restart unless-stopped \
  --gpus all \
  lapack-ai-prod:latest worker
```

## Troubleshooting

### Common Issues

#### Container Build Failures
```bash
# Check build context size
du -sh .

# Build with verbose output
docker build --progress=plain -f MODERNIZATION/_docs/Dockerfile.base .

# Clear build cache
docker builder prune -a
```

#### GPU Access Issues
```bash
# Verify GPU runtime
docker run --rm --gpus all nvidia/cuda:11.8-runtime-ubuntu20.04 nvidia-smi

# Check container GPU access
docker run --rm --gpus all lapack-ai-dev:latest python -c "import pyopencl; print('GPU OK')"

# Debug GPU permissions
docker run -it --gpus all --privileged lapack-ai-dev:latest bash
```

#### Performance Issues
```bash
# Monitor container resources
docker stats

# Check container logs
docker logs -f container-name

# Profile application in container
docker exec -it container-name python -m cProfile script.py
```

#### Volume Mount Issues
```bash
# Check file permissions
ls -la $(pwd)

# Fix ownership if needed (Linux)
sudo chown -R $USER:$USER .

# Alternative: run container as current user
docker run --user $(id -u):$(id -g) ...
```

## Development Best Practices

### Code Quality in Containers

```bash
# Run full quality check
docker run --rm -v $(pwd):/opt/lapack-ai lapack-ai-dev:latest bash -c "
cd /opt/lapack-ai
python -m black --check src/
python -m isort --check-only src/
python -m flake8 src/
python -m mypy src/
python -m pytest tests/
"
```

### Debugging in Containers

```bash
# Debug with debugger
docker run -it --rm \
  -v $(pwd):/opt/lapack-ai \
  --gpus all \
  lapack-ai-dev:latest python -m pdb script.py

# Debug with IPython
docker run -it --rm \
  -v $(pwd):/opt/lapack-ai \
  --gpus all \
  lapack-ai-dev:latest ipython
```

### Environment Consistency

```bash
# Ensure consistent environment
docker run --rm lapack-ai-dev:latest python -c "
import sys, platform
print(f'Python: {sys.version}')
print(f'Platform: {platform.platform()}')
print('Dependencies:')
import numpy, scipy, pyopencl, flask
print(f'  NumPy: {numpy.__version__}')
print(f'  SciPy: {scipy.__version__}')
print(f'  PyOpenCL: {pyopencl.VERSION_TEXT}')
print(f'  Flask: {flask.__version__}')
"
```

## Container Management

### Cleanup Commands

```bash
# Remove stopped containers
docker container prune

# Remove unused images
docker image prune

# Remove everything
docker system prune -a

# Clean up volumes
docker volume prune
```

### Container Backup

```bash
# Export container state
docker commit lapack-dev lapack-ai-dev:backup
docker save lapack-ai-dev:backup | gzip > lapack-dev-backup.tar.gz

# Import container state
gunzip -c lapack-dev-backup.tar.gz | docker load
```

## Next Steps

1. **Start Development**: Run development container and access Jupyter/Flask
2. **GPU Programming**: Develop OpenCL kernels within container environment
3. **Python Integration**: Create pybind11 wrappers using container build tools
4. **Dashboard Development**: Build Flask interface accessible via container ports
5. **Production Deployment**: Use production container for cloud deployment

## Container Structure

```
Container File System:
/opt/lapack-ai/                    # Working directory
├── src/                           # Source code (mounted from host)
├── build/                         # Build artifacts
├── tests/                         # Test files (mounted from host)
├── notebooks/                     # Jupyter notebooks
├── logs/                          # Application logs
└── data/                          # Data files

Container Services:
├── Jupyter Lab                    # Port 8888
├── Flask Development             # Port 5000
├── Monitoring Dashboard          # Port 5001
└── SSH (optional)                # Port 22
```

## Support

For containerized environment issues:
1. Verify Docker installation and GPU support
2. Check container build logs for dependency issues
3. Test GPU access in container environment
4. Review container resource limits and permissions
5. Use container debugging tools and verbose logging

---

**Environment Status**: ✅ Fully Containerized  
**Last Updated**: January 2025  
**Version**: 2.0.0 (Containerized)  
**Compatibility**: Docker 20.10+, OrbStack, GPU Runtime  
**Container Base**: python:3.11-slim  
**Production Ready**: ✅ 