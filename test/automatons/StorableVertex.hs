module StorableVertex
    (vertexSpecs
    ) where


import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr

import Linear
import Test.Hspec

import Yage.Resources (Vertex(..))


vertexSpecs :: Spec
vertexSpecs = do
    describe "storable vertex" $ do
        let fixureVertex = Vertex
                { position  = V4 2.0 3.0 5.0 7.0 
                , normal    = V4 11.0 13.0 17.0 19.0
                }

        it "can store a vertex to pointer and read it from this pointer" $ do
            ptr <- malloc :: IO (Ptr Vertex)
            poke ptr fixureVertex
            rVertex <- peek ptr
            free ptr
            rVertex `shouldBe` fixureVertex

