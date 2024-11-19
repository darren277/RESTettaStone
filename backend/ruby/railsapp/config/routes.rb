Rails.application.routes.draw do
  defaults format: :json do
    resources :users, only: [:index, :show, :create, :update, :destroy]
  end
end
