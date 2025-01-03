import {USER_API_BASE_URL} from "../../../config";

export default async function handler(req, res) {
    const { method } = req

    if (method === 'GET') {
        const data = await fetch(`${USER_API_BASE_URL}/users`);
        const json = await data.json();

        const users = json.map(user => ({id: user.id, email: user.email}));

        res.status(200).json(users);
    } else if (method === 'POST') {
        const { email } = req.body;

        if (!email) {
            return res.status(400).json({ message: 'Email is required' });
        }

        const response = await fetch(`${USER_API_BASE_URL}/users`, {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({ email })
        });

        if (!response.ok) {
            return res.status(response.status).json({ message: 'Failed to create user' });
        }

        const newUser = await response.json();
        res.status(200).json(newUser);
    } else {
        res.setHeader('Allow', ['GET', 'POST']);
        res.status(405).end(`Method ${method} Not Allowed`);
    }
}
