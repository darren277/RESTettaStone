Rails.application.routes.draw do
  defaults format: :json do
    namespace :api do
      resources :users, only: [:index, :show, :create, :update, :destroy]
    end
  end
end
