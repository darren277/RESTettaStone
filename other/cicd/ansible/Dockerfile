FROM ubuntu:20.04

# Install necessary dependencies
RUN apt-get update && apt-get install -y \
    software-properties-common \
    ansible \
    sshpass \
    python3-pip \
    git \
    curl \
    && apt-get clean

# Install Docker CLI for interacting with Docker on the host
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - && \
    add-apt-repository \
    "deb [arch=amd64] https://download.docker.com/linux/ubuntu focal stable" && \
    apt-get update && \
    apt-get install -y docker-ce-cli

# Set up directories for Ansible
WORKDIR /ansible
COPY . /ansible

# Expose default Ansible directory
VOLUME ["/ansible"]

# Entrypoint to Ansible
ENTRYPOINT ["ansible-playbook"]
