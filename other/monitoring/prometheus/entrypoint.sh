#!/bin/sh

# Replace placeholders in prometheus.template.yml with environment variables
sed -i "s/__FLASKAPP_IP__/${FLASKAPP_IP}/g" /etc/prometheus/prometheus.template.yml
sed -i "s/__FLASKAPP_PORT__/${FLASKAPP_PORT}/g" /etc/prometheus/prometheus.template.yml

# Move the processed file to prometheus.yml
cp /etc/prometheus/prometheus.template.yml /etc/prometheus/prometheus.yml

# Start Prometheus with the correct configuration
exec prometheus "$@"
