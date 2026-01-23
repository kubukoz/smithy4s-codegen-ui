# Generate Smithy4s code on the fly <!-- omit in toc -->

This repository is a maintained fork of [daddykotex/smithy4s-code-generation](https://github.com/daddykotex/smithy4s-code-generation).

- [Modules](#modules)
  - [Backend](#backend)
    - [Configuration](#configuration)
  - [Frontend](#frontend)
  - [Developement](#developement)
- [Dockerhub](#dockerhub)
- [Fly.io](#flyio)


## Modules

### Backend

Full stack Scala application with a Scalajs frontend. Depends on [Frontend](#frontend). This application is built on on top of Smithy4s and is deployed as a Docker container on fly.io.

Live at: https://smithy4s-codegen-ui.fly.dev/

Deployed on a very small 2vcpu and 512mb vm.

#### Configuration

The backend can be configured.

1. `SMITHY_CLASSPATH_CONFIG`: location of a json file used to load dependencies to include during code gen. The file content looks like this:

```json
{
  "entries": {
    "alloy-core": {
      "artifactId": "com.disneystreaming.alloy:alloy-core:0.2.8",
      "file": "/opt/docker/smithy-classpath/alloy-core-0.2.8.jar"
    },
    "smithy4s-protocol": {
      "artifactId": "com.disneystreaming.smithy4s:smithy4s-protocol:0.18.46",
      "file": "/opt/docker/smithy-classpath/smithy4s-protocol-0.18.46.jar"
    }
  }
}
```

Each entry has a friendly name (the key) and contains:
- `artifactId`: The full Maven coordinates (shown in the UI)
- `file`: The path to the JAR file

### Frontend

ScalaJS application shipped in [Backend](#backend).

### Developement

It is recomended that you have 3 long running proccesses to develop this application:

1. backend: `sbt ~backend/reStart`
2. cd into `modules/frontend` and run `npm i && npm run dev`
3. frontend scalajs: `sbt "~frontend/fastLinkJS"`

## Dockerhub

The images are pushed to the [docker hub](https://hub.docker.com/repository/docker/kubukoz/smithy4s-code-generation/general) so you can deploy them on your own infrastructure.

* latest: kubukoz/smithy4s-code-generation:latest
* kubukoz/smithy4s-code-generation:$SHA

## Fly.io

The images are pushed to the fly.io registry so that it can be deployed quickly from there.

Use the following command to use the right version of flyctl via nix:
`nix-shell -p flyctl -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/555bd32eb477d657e133ad14a5f99ac685bfdd61.tar.gz`
