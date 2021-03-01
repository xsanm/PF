unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = 
    let l = sqrt(a^2 + b^2)
    in (a / l, b / l)
    

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a, b, c) = 
    let l = sqrt(a^2 + b^2 + c^2)
    in (a / l, b / l, c / l)

triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = 
    let p = (a + b + c) / 2
    in sqrt(p * (p - a) * (p - b) * (p - c))

roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where {d = sqrt (b * b - 4 * a * c); e = 2 * a}