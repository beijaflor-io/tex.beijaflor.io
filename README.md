# tex.beijaflor.io
TeX PDF generation web service.

![](/screenshot.png)

## Problem
A TeX installation is very large and it's nice to separate the role of running
tex to a dedicated service.

## Usage (CLI)
```bash
curl -XPOST -F "file=@./input.tex" > output.pdf
```
The request to `tex.beijaflor.io` responds with a redirect to the Amazon S3
location of the file. It's your choice to follow the redirect to display the PDF
or to manually read response headers for more information.

We set headers:

- `x-sts-pdf` - The S3 PDF url
- `x-sts-log` - The S3 TeX log url
- `x-sts-log` - The S3 TeX input file url

The files are always at `s3://bucket/<MD5 of the Contents>` so services plugging
into this can generate the PDF URL based on the contents, or regenerate PDFs at
no cost by re-POSTing the same contents.

## Usage (JavaScript)
Additionally to accepting a `file` multipart form data parameter with an input
file, **tex.beijaflor.io** accepts a `text` form parameter with the TeX file as
an URLEncoded string.
```javascript
import fetch from 'isomorphic-fetch';
const latex = '...';
fetch('https://tex.beijaflor.io', {
  method: 'post',
  headers: {
    'content-type': 'application/x-www-form-urlencoded',
  },
  mode: 'cors',
  body: querystring.stringify({ text: latex }),
}).then(res => {
  const s3Location = res.url;
  // ... show PDF in iframe or show link for the users to download the result
});
```

## Installation
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
  # The public URL for the service
  -e STS_HOST=... \
  beijaflorio/tex.beijaflor.io
```

## Other
- https://github.com/tectonic-typesetting/tectonic/
