module BMI where

type BMI = Double

stdBMI :: BMI
stdBMI = 22.0

type Height = Double
type Weight = Double

-- 創りたいものを*先に*書く
bmi :: (Height, Weight) -> BMI
bmi (h, w) = w / h^2

mkbmi :: Height -> (Weight -> BMI)
mkbmi h = \w -> bmi(h, w)
