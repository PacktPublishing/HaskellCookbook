{-# LANGUAGE Rank2Types #-}



type Getter s a = s -> a

type Setter s b t = s -> b -> t

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a



lens :: Getter s a -> Setter s b t -> Lens s t a b
lens getter setter f x = fmap (setter x) $ f $ getter $ x


data Point = Point { x :: Double, y :: Double } deriving Show

xLens :: Lens' Point Double
xLens = lens x (\p t -> p { x = t })

yLens :: Lens' Point Double
yLens = lens y (\p t -> p { y = t })

data Access a s = Access a
                deriving Show

extract :: Access a s -> a
extract (Access a)  = a

instance Functor (Access a) where
    fmap _ (Access a) = Access a 

view :: Lens' s a -> s -> a 
view l = extract . l Access 


