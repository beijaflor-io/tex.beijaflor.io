# tex.beijaflor.io
TeX PDF generation as a service.

## Problem
A TeX installation is very large and it's nice to separate the role of running
tex to a dedicated service.

## Usage
Distributed as a docker image with tex installed plus the haskell server binary.

```
docker pull beijaflorio/tex.beijaflor.io
docker run -it \
  -p 80 \
  -e AWS_ACCESS_KEY_ID=... \
  -e AWS_SECRET_ACCESS_KEY=... \
  -e AWS_DEFAULT_REGION=... \
  # The name of the bucket to use to serve and cache PDF and output files
  -e STS_BUCKET_NAME=... \
  beijaflorio/tex.beijaflor.io
```

## Other
- https://github.com/tectonic-typesetting/tectonic/
