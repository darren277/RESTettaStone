<?php
namespace App\Controller;

use App\Entity\User;
use Doctrine\ORM\EntityManagerInterface;
use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\Routing\Annotation\Route;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Serializer\SerializerInterface;

class UserController extends AbstractController
{
    #[Route('/users', name: 'users')]
    public function hello(EntityManagerInterface $entityManager, SerializerInterface $serializer): Response
    {
        ob_start(); // Start output buffering

        $repository = $entityManager->getRepository(User::class);

        $response = new Response();$users = $repository->findAll();
        $jsonContent = $serializer->serialize($users, 'json');
        $response = new Response($jsonContent, Response::HTTP_OK, ['Content-Type' => 'application/json',]);

        ob_end_clean(); // End output buffering

        return $response;
    }
}
?>
