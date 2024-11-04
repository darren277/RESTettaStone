from locust import HttpUser, task, between, TaskSet

class UserBehavior(TaskSet):
    wait_time = between(1, 2)

    # def on_start(self): self.client.post("/login", json={"username":"foo", "password":"bar"})

    @task
    def index(self):
        #self.client.get("/")
        self.client.get('/users')

    # @task(3)
    # def view_item(self):
    #     for item_id in range(10):
    #         self.client.get(f"/item?id={item_id}", name="/item")


class WebsiteUser(HttpUser):
    tasks = [UserBehavior]

    # Define a wait time between tasks
    wait_time = between(1, 5)
