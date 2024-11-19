from django.db import models


class User(models.Model):
    # email = models.TextField()
    email = models.EmailField()

    class Meta:
        db_table = 'users'

    def __str__(self):
        return f"User: {self.email}"

    def toJSON(self):
        return dict(id=self.id, email=self.email)

