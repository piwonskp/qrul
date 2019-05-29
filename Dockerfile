FROM haskell:8.6.5

RUN apt-get update && apt-get install wget
RUN echo "deb http://apt.llvm.org/stretch/ llvm-toolchain-stretch-8 main" | tee -a /etc/apt/sources.list
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|apt-key add -
RUN apt-get update && \
    apt-get install --assume-yes libllvm-8-ocaml-dev libllvm8 llvm-8 llvm-8-dev llvm-8-doc llvm-8-examples llvm-8-runtime

WORKDIR /opt/app
COPY package.yaml /opt/app
COPY stack.yaml /opt/app
RUN stack install --only-dependencies

COPY . /opt/app
RUN stack install
