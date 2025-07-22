# Docker Configuration and Deployment Guide
**LAPACK AI Modernization Project**

## Overview

This document provides comprehensive guidance for Docker container configuration, usage, and deployment for the LAPACK AI modernization project. The Docker infrastructure supports both development and production environments with GPU acceleration capabilities.

## Container Architecture

### Base Image (`Dockerfile.base`)
- **Purpose**: Foundation image with all system dependencies
- **Base**: `python:3.11-slim`
- **Size**: ~350MB (optimized for minimal footprint)
- **Features**:
  - Complete build toolchain (GCC, Gfortran, CMake)
  - OpenCL runtime and headers
  - BLAS/LAPACK system libraries
  - Python 3.11 with scientific computing stack
  - Non-root user configuration for security

### Development Image (`Dockerfile.dev`)
- **Purpose**: Full development environment with debugging tools
- **Base**: `lapack-ai-base:latest`
- **Size**: ~450MB
- **Features**:
  - Jupyter Lab with extensions
  - Development tools (vim, htop, debugging tools)
  - Flask development server
  - Volume mounts for live code editing
  - GPU passthrough support
  - Interactive development startup script

### Production Image (`Dockerfile.prod`)
- **Purpose**: Optimized production deployment
- **Base**: Multi-stage build from `python:3.11-slim`
- **Size**: <500MB (target achieved through optimization)
- **Features**:
  - Minimal runtime dependencies only
  - Gunicorn WSGI server
  - Health check endpoints
  - Security hardening
  - Efficient layer caching

## Build Instructions

### Prerequisites
```bash
# Ensure Docker is installed and running
docker --version
docker-compose --version

# For GPU support, install nvidia-docker2 (Linux)
# https://github.com/NVIDIA/nvidia-docker
```

### Building Images

#### 1. Build Base Image
```bash
# From project root
cd /path/to/lapack_ai

# Build base image
docker build -f MODERNIZATION/_docs/Dockerfile.base -t lapack-ai-base:latest .

# Verify build
docker images | grep lapack-ai-base
```

#### 2. Build Development Image
```bash
# Build development image
docker build -f MODERNIZATION/dev_environment/Dockerfile.dev -t lapack-ai-dev:latest .

# Verify development environment
docker run --rm lapack-ai-dev:latest python -c "import numpy, scipy, pyopencl; print('Development environment ready')"
```

#### 3. Build Production Image
```bash
# Build production image
docker build -f MODERNIZATION/_docs/Dockerfile.prod -t lapack-ai-prod:latest .

# Check image size
docker images lapack-ai-prod:latest
```

## Container Usage

### Development Environment

#### Interactive Development
```bash
# Start interactive development container
docker run -it --rm \
  --name lapack-dev \
  -v $(pwd):/opt/lapack-ai \
  -p 8888:8888 \
  -p 5000:5000 \
  -p 5001:5001 \
  --gpus all \
  lapack-ai-dev:latest

# Available commands in container:
# jupyter  - Start Jupyter Lab
# flask    - Start Flask development server  
# test     - Run test suite
# build    - Build LAPACK
```

#### Jupyter Lab Development
```bash
# Start Jupyter Lab server
docker run -d \
  --name lapack-jupyter \
  -v $(pwd):/opt/lapack-ai \
  -p 8888:8888 \
  --gpus all \
  lapack-ai-dev:latest jupyter

# Access at: http://localhost:8888
```

#### Flask Development Server
```bash
# Start Flask development server
docker run -d \
  --name lapack-flask-dev \
  -v $(pwd):/opt/lapack-ai \
  -p 5000:5000 \
  --gpus all \
  lapack-ai-dev:latest flask

# Access at: http://localhost:5000
```

### Production Deployment

#### Single Container
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

#### Docker Compose (Recommended)
```yaml
# docker-compose.yml
version: '3.8'

services:
  web:
    image: lapack-ai-prod:latest
    ports:
      - "5000:5000"
    command: web
    restart: unless-stopped
    deploy:
      resources:
        reservations:
          devices:
            - driver: nvidia
              count: 1
              capabilities: [gpu]
    healthcheck:
      test: ["CMD", "/opt/lapack-ai/start.sh", "health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s

  worker:
    image: lapack-ai-prod:latest
    command: worker
    restart: unless-stopped
    deploy:
      resources:
        reservations:
          devices:
            - driver: nvidia
              count: 1
              capabilities: [gpu]

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
    depends_on:
      - web
    restart: unless-stopped
```

```bash
# Deploy with Docker Compose
docker-compose up -d

# Scale workers
docker-compose up -d --scale worker=3

# Monitor logs
docker-compose logs -f web worker
```

## GPU Configuration

### NVIDIA GPU Support
```bash
# Verify GPU access in container
docker run --rm --gpus all lapack-ai-dev:latest \
  python -c "
import pyopencl as cl
platforms = cl.get_platforms()
print(f'OpenCL platforms: {len(platforms)}')
for platform in platforms:
    devices = platform.get_devices()
    print(f'Platform: {platform.name}, Devices: {len(devices)}')
    for device in devices:
        print(f'  Device: {device.name}')
"

# Check NVIDIA runtime
docker run --rm --gpus all nvidia/cuda:11.8-runtime-ubuntu20.04 nvidia-smi
```

### AMD GPU Support (ROCm)
```bash
# For AMD GPUs, use ROCm runtime
docker run --rm \
  --device=/dev/kfd \
  --device=/dev/dri \
  --group-add video \
  lapack-ai-dev:latest clinfo
```

## Health Checks and Monitoring

### Container Health Checks
```bash
# Check container health
docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"

# Inspect health check details
docker inspect lapack-ai-web | jq '.[0].State.Health'

# Manual health check
docker exec lapack-ai-web /opt/lapack-ai/start.sh health
```

### Monitoring Endpoints
```bash
# Application health
curl http://localhost:5000/health

# System metrics
curl http://localhost:5001/metrics

# OpenCL device status
curl http://localhost:5000/gpu/status
```

## Performance Optimization

### Image Size Optimization
```bash
# Analyze image layers
docker history lapack-ai-prod:latest

# Remove intermediate containers
docker system prune -f

# Squash layers (experimental)
docker build --squash -f MODERNIZATION/_docs/Dockerfile.prod -t lapack-ai-prod:squashed .
```

### Build Cache Optimization
```bash
# Build with cache mount (BuildKit)
export DOCKER_BUILDKIT=1
docker build \
  --mount=type=cache,target=/var/cache/apt \
  --mount=type=cache,target=/root/.cache/pip \
  -f MODERNIZATION/_docs/Dockerfile.prod \
  -t lapack-ai-prod:latest .
```

## Cloud Deployment

### AWS ECS
```json
{
  "family": "lapack-ai",
  "taskRoleArn": "arn:aws:iam::account:role/ecsTaskRole",
  "requiresCompatibilities": ["EC2"],
  "cpu": "2048",
  "memory": "4096",
  "containerDefinitions": [
    {
      "name": "lapack-ai-web",
      "image": "your-registry/lapack-ai-prod:latest",
      "portMappings": [
        {
          "containerPort": 5000,
          "protocol": "tcp"
        }
      ],
      "resourceRequirements": [
        {
          "type": "GPU",
          "value": "1"
        }
      ],
      "logConfiguration": {
        "logDriver": "awslogs",
        "options": {
          "awslogs-group": "/ecs/lapack-ai",
          "awslogs-region": "us-west-2"
        }
      }
    }
  ]
}
```

### Google Cloud Run
```yaml
apiVersion: serving.knative.dev/v1
kind: Service
metadata:
  name: lapack-ai
  annotations:
    run.googleapis.com/gpu-type: nvidia-tesla-t4
spec:
  template:
    metadata:
      annotations:
        run.googleapis.com/execution-environment: gen2
        run.googleapis.com/gpu-type: nvidia-tesla-t4
    spec:
      containers:
      - image: gcr.io/your-project/lapack-ai-prod:latest
        ports:
        - containerPort: 5000
        resources:
          limits:
            memory: 4Gi
            cpu: 2
            nvidia.com/gpu: 1
```

### Azure Container Instances
```bash
az container create \
  --resource-group lapack-ai-rg \
  --name lapack-ai-web \
  --image your-registry/lapack-ai-prod:latest \
  --ports 5000 \
  --protocol TCP \
  --cpu 2 \
  --memory 4 \
  --gpu-count 1 \
  --gpu-sku V100
```

## Security Best Practices

### Container Security
- ✅ Non-root user (lapack:lapack)
- ✅ Minimal base image (python:3.11-slim)
- ✅ No secrets in image layers
- ✅ Read-only filesystem where possible
- ✅ Resource limits configured
- ✅ Health checks implemented

### Network Security
```bash
# Create custom network
docker network create --driver bridge lapack-network

# Run containers in isolated network
docker run -d \
  --name lapack-ai-web \
  --network lapack-network \
  -p 5000:5000 \
  lapack-ai-prod:latest
```

### Secrets Management
```bash
# Use Docker secrets (Swarm mode)
echo "db_password" | docker secret create db_password -

# Mount secrets in container
docker service create \
  --name lapack-ai \
  --secret db_password \
  lapack-ai-prod:latest
```

## Troubleshooting

### Common Issues

#### Build Failures
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
# Verify NVIDIA runtime
docker run --rm --gpus all nvidia/cuda:11.8-runtime-ubuntu20.04 nvidia-smi

# Check OpenCL devices
docker run --rm --gpus all lapack-ai-dev:latest clinfo

# Debug OpenCL in container
docker run -it --gpus all lapack-ai-dev:latest bash
# Inside container:
export OCL_ENABLE_DEBUG=1
python -c "import pyopencl as cl; print(cl.get_platforms())"
```

#### Performance Issues
```bash
# Monitor container resources
docker stats lapack-ai-web

# Check container logs
docker logs -f lapack-ai-web

# Profile application
docker exec -it lapack-ai-web python -m cProfile -o profile.stats your_script.py
```

## Development Workflow

### Recommended Development Cycle
1. **Local Development**: Use development container with volume mounts
2. **Testing**: Run test suite in clean container
3. **Integration**: Test with production container locally
4. **Deployment**: Push to registry and deploy to cloud

### CI/CD Integration
```yaml
# .github/workflows/docker.yml
name: Docker Build and Push

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Set up Docker Buildx
      uses: docker/setup-buildx-action@v2
    
    - name: Build base image
      run: |
        docker build -f MODERNIZATION/_docs/Dockerfile.base -t lapack-ai-base:latest .
    
    - name: Build production image
      run: |
        docker build -f MODERNIZATION/_docs/Dockerfile.prod -t lapack-ai-prod:latest .
    
    - name: Test container
      run: |
        docker run --rm lapack-ai-prod:latest health
```

This Docker configuration provides a robust, scalable, and secure foundation for LAPACK AI development and deployment across multiple environments. 