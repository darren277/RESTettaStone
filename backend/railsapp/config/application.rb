require_relative 'boot'

require "rails"

require "active_model/railtie"
require "active_job/railtie"
require "active_record/railtie"
require "active_storage/engine"
require "action_controller/railtie"
require "action_mailer/railtie"
require "action_view/railtie"

Bundler.require(*Rails.groups)

module MyApi
  class Application < Rails::Application
    config.load_defaults 5.2
    config.api_only = true

    config.cache_classes = false
    config.eager_load = false
    config.consider_all_requests_local = true

    if Rails.root.join('tmp', 'caching-dev.txt').exist?
        config.action_controller.perform_caching = true
        config.cache_store = :memory_store
        config.public_file_server.headers = {'Cache-Control' => "public, max-age=#{2.days.to_i}"}
    else
        config.action_controller.perform_caching = false
        config.cache_store = :null_store
    end

    config.active_storage.service = :local
    config.action_mailer.raise_delivery_errors = false
    config.action_mailer.perform_caching = false
    config.active_support.deprecation = :log
    config.active_record.migration_error = :page_load
    config.active_record.verbose_query_logs = true
    config.file_watcher = ActiveSupport::EventedFileUpdateChecker
  end
end
