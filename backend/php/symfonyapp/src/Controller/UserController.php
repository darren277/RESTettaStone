<?php
namespace App\Controller;

use App\Entity\User;
use Doctrine\ORM\EntityManagerInterface;
use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\Routing\Annotation\Route;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Serializer\SerializerInterface;
use Symfony\Component\Validator\Validator\ValidatorInterface;

class UserController extends AbstractController
{
    #[Route('/users', name: 'users', methods: ['GET', 'POST'])]
    public function handleUsers(Request $request, EntityManagerInterface $entityManager, SerializerInterface $serializer, ValidatorInterface $validator): Response
    {
        ob_start(); // Start output buffering

        if ($request->isMethod('GET')) {
            // List all users
            $users = $entityManager->getRepository(User::class)->findAll();
            $data = $serializer->serialize($users, 'json');
            $response = new Response($data, 200, ['Content-Type' => 'application/json']);
            ob_end_clean(); // End output buffering
            return $response;
        }

        if ($request->isMethod('POST')) {
            // Create a new user
            $data = $request->getContent();
            $user = $serializer->deserialize($data, User::class, 'json');

            // Validate the user
            $errors = $validator->validate($user);
            if (count($errors) > 0) {
                //return $this->json($errors, 400);
                $response = new Response();
                $response->setContent(json_encode($errors));
                $response->setStatusCode(Response::HTTP_BAD_REQUEST);
                $response->headers->set('Content-Type', 'application/json');
                ob_end_clean(); // End output buffering
                return $response;
            }

            $entityManager->persist($user);
            $entityManager->flush();

            $response = new Response();
            $response->setContent(json_encode($user));
            $response->setStatusCode(Response::HTTP_OK);
            $response->headers->set('Content-Type', 'application/json');
            ob_end_clean(); // End output buffering
            return $response;
        }

        return $this->json(['error' => 'Method not allowed'], 405);
    }

    #[Route('/users/{id}', name: 'user', methods: ['GET', 'PUT', 'DELETE'])]
    public function handleUser($id, Request $request, EntityManagerInterface $entityManager, SerializerInterface $serializer, ValidatorInterface $validator): Response
    {
        ob_start(); // Start output buffering

        $user = $entityManager->getRepository(User::class)->find($id);

        if (!$user) {
            //return $this->json(['error' => 'User not found'], 404);
            $response = new Response();
            $response->setContent(json_encode(['error' => 'User not found']));
            $response->setStatusCode(Response::HTTP_NOT_FOUND);
            $response->headers->set('Content-Type', 'application/json');
            ob_end_clean(); // End output buffering
            return $response;
        }

        if ($request->isMethod('GET')) {
            // Get a specific user
            $data = $serializer->serialize($user, 'json');
            $response = new Response($data, 200, ['Content-Type' => 'application/json']);
            ob_end_clean(); // End output buffering
            return $response;
        }

        if ($request->isMethod('PUT')) {
            // Update a user
            $data = $request->getContent();
            $serializer->deserialize($data, User::class, 'json', ['object_to_populate' => $user]);

            // Validate the user
            $errors = $validator->validate($user);
            if (count($errors) > 0) {
                //return $this->json($errors, 400);
                $response = new Response();
                $response->setContent(json_encode($errors));
                $response->setStatusCode(Response::HTTP_BAD_REQUEST);
                $response->headers->set('Content-Type', 'application/json');
                ob_end_clean(); // End output buffering
                return $response;
            }

            $entityManager->flush();

            //return $this->json($user);
            $response = new Response();
            $response->setContent(json_encode($user));
            $response->setStatusCode(Response::HTTP_OK);
            $response->headers->set('Content-Type', 'application/json');
            ob_end_clean(); // End output buffering
            return $response;
        }

        if ($request->isMethod('DELETE')) {
            // Delete a user
            $entityManager->remove($user);
            $entityManager->flush();

            //return $this->json(['message' => 'User successfully deleted']);
            $response = new Response();
            $response->setContent(json_encode(['message' => 'User successfully deleted']));
            $response->setStatusCode(Response::HTTP_OK);
            $response->headers->set('Content-Type', 'application/json');
            ob_end_clean(); // End output buffering
            return $response;
        }

        //return $this->json(['error' => 'Method not allowed'], 405);
        $response = new Response();
        $response->setContent(json_encode(['error' => 'Method not allowed']));
        $response->setStatusCode(Response::HTTP_METHOD_NOT_ALLOWED);
        $response->headers->set('Content-Type', 'application/json');
        ob_end_clean(); // End output buffering
        return $response;
    }
}
?>
