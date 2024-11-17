""""""
from dotenv import dotenv_values

import pika


config = dotenv_values(".env")


class RMQTest:
    """
    This class will test the RabbitMQ connection and all of its CRUD functionality.
    """

    def __init__(self, rmq_host: str, rmq_port: int):
        self._rmq_host = rmq_host
        self._rmq_port = rmq_port

        self._connection = None
        self._channel = None
        self._queue = None

    def connect(self):
        """
        This method will connect to the RabbitMQ server.
        """
        self._connection = pika.BlockingConnection(pika.ConnectionParameters(self._rmq_host, self._rmq_port))
        self._channel = self._connection.channel()

    def create_queue(self, queue_name: str):
        """
        This method will create a queue in the RabbitMQ server.
        """
        self._channel.queue_declare(queue=queue_name)
        self._queue = queue_name

    def publish_message(self, message: str):
        """
        This method will publish a message to the RabbitMQ server.
        """
        self._channel.basic_publish(exchange='', routing_key=self._queue, body=message)

    def consume_message(self):
        """
        This method will consume a message from the RabbitMQ server.
        """
        method_frame, header_frame, body = self._channel.basic_get(queue=self._queue, auto_ack=True)
        if body:
            return body.decode('utf-8')
        else:
            return None

    def close(self):
        """
        This method will close the connection to the RabbitMQ server.
        """
        self._connection.close()

    def test_rmq(self):
        """
        This method will test the RabbitMQ connection and all of its CRUD functionality.
        """
        self.connect()
        self.create_queue('test_queue')
        self.publish_message('Hello, World!')
        body = self.consume_message()
        self.close()

        if body:
            return f'Received message: {body}'
        else:
            return 'No message receive'


if __name__ == '__main__':
    rmq_test = RMQTest(config.get('RMQ_HOST', 'localhost'), config.get('RMQ_PORT', 5672))
    print(rmq_test.test_rmq())
