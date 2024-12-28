import {USER_API_BASE_URL} from "../../../config";

export default async function handler(req, res) {
     const {query: { id }, method} = req;

    if (method === 'GET') {
        const data = await fetch(`${USER_API_BASE_URL}/users`);
        const json = await data.json();

        const users = json.map(user => ({id: user.id, email: user.email}));

        if (!users) {
            return res.status(404).json({ message: `User with id ${id} not found` });
        }

        res.status(200).json(users);
    } else if (method === 'PUT') {
        const { email } = req.body;

        if (!email) {
            return res.status(400).json({ message: 'Email is required' });
        }

        const data = await fetch(`${USER_API_BASE_URL}/users`);
        const json = await data.json();
        const user = json.find(user => user.id === id);

        if (!user) {
            return res.status(404).json({ message: `User with id ${id} not found` });
        };

        const response = await fetch(`${USER_API_BASE_URL}/users/${id}`, {
            method: 'PUT',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ email }),
        });

        if (!response.ok) {
            return res
                .status(response.status)
                .json({ message: 'Failed to update user' });
        }

        const updatedUser = await response.json();
        return res.status(200).json(updatedUser);
    } else if (method === 'DELETE') {
        const data = await fetch(`${USER_API_BASE_URL}/users`);
        const json = await data.json();
        const user = json.find(user => user.id === id);

        if (!user) {
            return res.status(404).json({ message: `User with id ${id} not found` });
        }

        const response = await fetch(`${USER_API_BASE_URL}/users/${id}`, {
            method: 'DELETE',
        });

        if (!response.ok) {
            return res
                .status(response.status)
                .json({ message: 'Failed to delete user' });
        }

        return res.status(204).end();
    } else {
        res.setHeader('Allow', ['GET', 'PUT', 'DELETE']);
        res.status(405).end(`Method ${method} Not Allowed`);
    }
}
