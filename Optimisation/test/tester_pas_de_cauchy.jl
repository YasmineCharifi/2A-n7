@doc doc"""
Tester l'algorithme du pas de cauchy

# Entrées :
   * afficher : (Bool) affichage ou non des résultats de chaque test

# Les cas de test (dans l'ordre)
   * la quadratique 1
   * la quadratique 2
   * la quadratique 3
   * la quadratique 4

"""
function tester_pas_de_cauchy(affiche::Bool,Pas_De_Cauchy::Function)

     # Tolérance utilisé dans les tests
     tol_test = 1e-3

    @testset "cauchy" begin
        # le cas de test 1 (b = 0)
        grad = [0 ; 0]
        Hess = [7 0 ; 0 2]
        delta = 1
        s,e = Pas_De_Cauchy(grad,Hess,delta)
        @test  s ≈ [0.0 ; 0.0] atol = tol_test
        @test e == 0

        # le cas de test 2 (b != 0 et a > 0)
        grad = [6 ; 2]
        Hess = [7 0 ; 0 2]
        delta = 0.5       # sol = pas de Cauchy  
        s,e = Pas_De_Cauchy(grad,Hess,delta) #norm(s) > delta
        @test  s ≈ [-0.4743416490252569, -0.15811388300841897] atol = tol_test
        @test e == -1
        delta = 1.2       # saturation à la 2ieme itération  
        s,e = Pas_De_Cauchy(grad,Hess,delta) #norm(s) <= delta
        @test  s ≈ [-0.9230769230769234, -0.30769230769230776] atol = tol_test
        @test e == 1


        # le cas de test 3
        grad = [-2 ; 3]
        Hess = [4 6 ; 6 5]
        delta = 3
        s,e = Pas_De_Cauchy(grad,Hess,delta)
        @test  s ≈ [1.6641005886756874, -2.4961508830135313]  atol = tol_test
        @test e == 1

        # le cas de test 4
        # Le pas de Cauchy conduit à un gradient nul en 1 itération
        grad = [2 ; 0]
        Hess = [4 0 ; 0 -15]
        delta = 0.1
        s,e = Pas_De_Cauchy(grad,Hess,delta)
        @test  s ≈ [-0.1, -0.0] atol = tol_test
        @test e == -1
    end
end
