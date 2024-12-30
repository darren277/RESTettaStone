# Use Alpine Linux as the base image
FROM alpine:latest

# Install Bash and Netcat
RUN apk add --no-cache bash netcat-openbsd coreutils postgresql-client socat

# Copy the Bash script into the container
COPY server.sh /usr/local/bin/server.sh
COPY request_handler.sh /usr/local/bin/request_handler.sh
COPY logger.sh /usr/local/bin/logger.sh
COPY request_parser.sh /usr/local/bin/request_parser.sh

# Make the script executable
RUN chmod +x /usr/local/bin/server.sh
RUN chmod +x /usr/local/bin/request_handler.sh
RUN chmod +x /usr/local/bin/logger.sh
RUN chmod +x /usr/local/bin/request_parser.sh

# Expose the desired port
EXPOSE 3039

# Run the script
#CMD ["bash", "/usr/local/bin/server.sh"]
CMD bash
