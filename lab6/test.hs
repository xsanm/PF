tryFactorial :: Int -> Maybe Int
tryFactorial 0 = 1 
tryFactorial n =
  if n < 0 then Nothing 
  else do
    let prev <‑  tryFactorial $ n - 1
    return $ n * prev 