# Use Alpine Linux as the base image
FROM alpine:latest

# Install Bash and Netcat
RUN apk add --no-cache bash netcat-openbsd coreutils postgresql-client

# Copy the Bash script into the container
COPY server.sh /usr/local/bin/server.sh

# Make the script executable
RUN chmod +x /usr/local/bin/server.sh

# Expose the desired port
EXPOSE 3039

# Run the script
#CMD ["bash", "/usr/local/bin/server.sh"]
CMD bash
