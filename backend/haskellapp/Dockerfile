FROM haskell:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    g++ \
    gcc \
    make \
    libgmp-dev \
    zlib1g-dev

# Install GHC if using system-wide GHC (optional)
# RUN apt-get install -y ghc libtinfo-dev zlib1g-dev

# Install Stack
#RUN curl -sSL https://get.haskellstack.org/ | sh

# Set up stack to use system GHC (optional, if using system-wide GHC)
# RUN stack config set system-ghc --global true

# install ghc separately using stack...
RUN stack setup --install-ghc

# Copy project files
#COPY . /haskellapp
COPY ./app/Main.hs /haskellapp/app/Main.hs
COPY ./src/Lib.hs /haskellapp/src/Lib.hs
COPY ./package.yaml /haskellapp/package.yaml
COPY ./stack.yaml /haskellapp/stack.yaml

WORKDIR /haskellapp

# Install dependencies and GHC if not using system GHC
#RUN stack setup && stack build --install-ghc --only-dependencies
RUN stack setup && stack build --only-dependencies

CMD ["bash"]

# Build the project
#RUN stack build

# Run the app
#CMD ["stack", "exec", "haskellapp-exe"]
