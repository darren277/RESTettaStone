import {USER_API_BASE_URL} from "../../../config";

export default async function handler({ query: { id } }, res) {
    const { method } = req

    if (method !== 'GET') {
        res.setHeader('Allow', ['GET'])
        res.status(405).end(`Method ${method} Not Allowed`)
    }

    const data = await fetch(`${USER_API_BASE_URL}/users`);
    const json = await data.json();

    const users = json.map(user => {
        return {
            id: user.id,
            email: user.email
        }
    });

    const filtered = users.filter((user) => user.id === id)

    if (filtered.length > 0) {
        res.status(200).json(filtered[0])
    } else {
        res.status(404).json({ message: `User with the id of ${id} is not found` })
    }
}
