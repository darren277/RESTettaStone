#!/bin/sh

# Replace variables in the Prometheus template
envsubst < /etc/prometheus/prometheus.template.yml > /etc/prometheus/prometheus.yml

# Start Prometheus with provided arguments
exec prometheus "$@"
