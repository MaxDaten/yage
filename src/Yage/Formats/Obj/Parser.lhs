not the full spec of the obj format, just the supported parts are included
in general free-form is not supported!
full spec: http://www.martinreddy.net/gfx/3d/OBJ.spec

> {-# LANGUAGE DeriveGeneric   #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-missing-signatures #-}
> module Yage.Formats.Obj.Parser where

> import Yage.Prelude   hiding ((<|>), try, Index, snoc, lines, ByteString, readFile)
> import Yage.Lens      hiding (Index, elements)

> import qualified Data.Vector as V
> import Text.Parsec hiding (Line)
> import Text.Parsec.ByteString.Lazy
> import Data.ByteString.Lazy hiding (zipWith, snoc)
> import Text.Read
> import Linear
> import Linear.DeepSeq ()

> import Generics.Deriving.Monoid
> import Control.DeepSeq.Generics

obj Parsing
===========

> data OBJ = OBJ
>   { _vertexData   :: OBJVertexData
>   , _elements     :: OBJElements
>   , _comments     :: [Comment]
>   , _name         :: Maybe ObjectName
>   } deriving ( Show, Eq, Generic )

> data OBJVertexData = OBJVertexData
>   { _geometricVertices :: (V.Vector GeoVertex)
>   , _textureVertices   :: (V.Vector TexVertex)
>   , _vertexNormals     :: (V.Vector VertexNormal)
>   } deriving ( Show, Eq, Generic )

> data OBJElements = OBJElements
>   { _points     :: (V.Vector Point)
>   , _lines      :: (V.Vector Line)
>   , _faces      :: (V.Vector Face) 
>   } deriving ( Show, Eq, Generic )


> type OBJParser t = GenParser t OBJ

File Structure
--------------

The following types of data may be included in an .obj file. In this
list, the keyword (in parentheses) follows the data type.

Vertex data
~~~~~~~~~~~

- geometric vertices (v)
- texture vertices (vt)
- vertex normals (vn)

The vertex data is represented by four vertex lists; one for each type
of vertex coordinate. A right-hand coordinate system is used to specify
the coordinate locations.

When vertices are loaded into the Advanced Visualizer, they are
sequentially numbered, starting with 1. These reference numbers are
used in element statements.

Elements
~~~~~~~~
- point (p)
- line (l)
- face (f)


Grouping
~~~~~~~~
- group name (g)
- object name (o)


Syntax
______

The following syntax statements are listed in order of complexity.

Vertex Data
~~~~~~~~~~~

- v x y z w

    Polygonal and free-form geometry statement.

    Specifies a geometric vertex and its x y z coordinates. Rational
    curves and surfaces require a fourth homogeneous coordinate, also
    called the weight.

    x y z are the x, y, and z coordinates for the vertex. These are
    **floating point** numbers that define the position of the vertex in
    three dimensions.

    w is the weight required for rational curves and surfaces. It is
    not required for non-rational curves and surfaces. If you do not
    specify a value for w, the default is 1.0.

    NOTE: A positive weight value is recommended. Using zero or
    negative values may result in an undefined point in a curve or
    surface.

> type GeoVertex = V3 Float

> geovertex :: OBJParser t GeoVertex
> geovertex = string "v " >> spaces >> v3

- vn i j k

    Polygonal and free-form geometry statement.

    Specifies a normal vector with components i, j, and k.

    Vertex normals affect the smooth-shading and rendering of geometry.
    For polygons, vertex normals are used in place of the actual facet
    normals.  For surfaces, vertex normals are interpolated over the
    entire surface and replace the actual analytic surface normal.

    When vertex normals are present, they supersede smoothing groups.

    i j k are the i, j, and k coordinates for the vertex normal. They
    are **floating point** numbers.

> type VertexNormal = V3 Float

> vertexnormal :: OBJParser t VertexNormal
> vertexnormal = string "vn " >> spaces >> v3

- vt u v w

    Vertex statement for both polygonal and free-form geometry.

    Specifies a texture vertex and its coordinates. A 1D texture
    requires only u texture coordinates, a 2D texture requires both u
    and v texture coordinates, and a 3D texture requires all three
    coordinates.

    u is the value for the _horizontal_ direction of the texture.

    v is an optional argument.

    v is the value for the _vertical_ direction of the texture. The
    default is 0.

    w is an optional argument.

    w is a value for the _depth_ of the texture. The default is 0.

> type TexVertex = V2 Float

> texvertex :: OBJParser t TexVertex
> texvertex = string "vt " >> spaces >> v2



Elements
~~~~~~~~

For polygonal geometry, the element types available in the .obj file
are:
- points
- lines
- faces


Referencing vertex data
~~~~~~~~~~~~~~~~~~~~~~~

For all elements, reference numbers are used to identify geometric
vertices, texture vertices, vertex normals.

Each of these types of vertices is numbered separately, starting with
1. This means that the first geometric vertex in the file is 1, the
second is 2, and so on. The first texture vertex in the file is 1, the
second is 2, and so on. The numbering continues sequentially throughout
the entire file. Frequently, files have _multiple_ lists of vertex data.
This numbering sequence continues even when vertex data is separated by
other data.

In addition to counting vertices down from the top of the first list in
the file, you can also count vertices back up the list from an
element's position in the file. When you count up the list from an
element, the reference numbers are negative. A reference number of -1
indicates the vertex immediately above the element. A reference number
of -2 indicates two references above and so on.

Referencing groups of vertices
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some elements, such as faces and surfaces, may have a triplet of
numbers that reference vertex data.These numbers are the reference
numbers for a geometric vertex, a texture vertex, and a vertex normal.

Each triplet of numbers specifies a geometric vertex, texture vertex,
and vertex normal. The reference numbers must be in order and must
separated by slashes (/).

- The first reference number is the geometric vertex.

- The second reference number is the texture vertex. It follows
    the first slash.

- The third reference number is the vertex normal. It follows the
    second slash.

There is no space between numbers and the slashes. There may be more
than one series of geometric vertex/texture vertex/vertex normal
numbers on a line.

The following is a portion of a sample file for a four-sided face
element:

    f 1/1/1 2/2/2 3/3/3 4/4/4

Using v, vt, and vn to represent geometric vertices, texture vertices,
and vertex normals, the statement would read:

    f v/vt/vn v/vt/vn v/vt/vn v/vt/vn

If there are only vertices and vertex normals for a face element (no
texture vertices), you would enter two slashes (//). For example, to
specify only the vertex and vertex normal reference numbers, you would
enter:

    f 1//1 2//2 3//3 4//4

When you are using a series of triplets, you must be _consistent_ in the
way you reference the vertex data. For example, it is illegal to give
vertex normals for some vertices, but not all.

The following is an example of an illegal statement.

    f 1/1/1 2/2/2 3//3 4//4

> data OBJIndex      = OBJVertexIndex Int | OBJTextureIndex Int | OBJNormalIndex Int
>   deriving ( Show, Eq, Generic )
> type References = [OBJIndex]


Syntax
------

The following syntax statements are listed in order of complexity of
geometry.

- p  v1 v2 v3 . . .

    Polygonal geometry statement.

    Specifies a point element and its vertex. You can specify multiple
    points with this statement. Although points cannot be shaded or
    rendered, they are used by other Advanced Visualizer programs.

    v is the vertex reference number for a point element. Each point
    element requires one vertex. Positive values indicate absolute
    vertex numbers. Negative values indicate relative vertex numbers.


> type Point = [OBJIndex]
> pointElements :: OBJParser t Point
> pointElements = string "p " >> manyTill (spaces >> OBJVertexIndex <$> int) eol


- l  v1/vt1   v2/vt2   v3/vt3 . . .

    Polygonal geometry statement.

    Specifies a line and its vertex reference numbers. You can
    optionally include the texture vertex reference numbers. Although
    lines cannot be shaded or rendered, they are used by other Advanced
    Visualizer programs.

    The reference numbers for the vertices and texture vertices must be
    separated by a slash (/). There is no space between the number and
    the slash.

    v is a reference number for a vertex on the line. A minimum of two
    vertex numbers are required. There is no limit on the maximum.
    Positive values indicate absolute vertex numbers. Negative values
    indicate relative vertex numbers.

    vt is an optional argument.

    vt is the reference number for a texture vertex in the line
    element. It must always follow the first slash.

> type Line = [References]
> lineElements :: OBJParser t Line
> lineElements = string "l " >> manyTill (spaces >> idxs <$> int `sepBy1` (char '/')) eol
>   where idxs = zipWith ($) [OBJVertexIndex, OBJTextureIndex] 

- f  v1/vt1/vn1   v2/vt2/vn2   v3/vt3/vn3 . . .

    Polygonal geometry statement.

    Specifies a face element and its vertex reference number. You can
    optionally include the texture vertex and vertex normal reference
    numbers.

    The reference numbers for the vertices, texture vertices, and
    vertex normals must be separated by slashes (/). There is no space
    between the number and the slash.

    v is the reference number for a vertex in the face element. A
    minimum of three vertices are required.

    vt is an optional argument.

    vt is the reference number for a texture vertex in the face
    element. It always follows the first slash.

    vn is an optional argument.

    vn is the reference number for a vertex normal in the face element.
    It must always follow the second slash.

    Face elements use surface normals to indicate their orientation. If
    vertices are ordered counterclockwise around the face, both the
    face and the normal will point toward the viewer. If the vertex
    ordering is clockwise, both will point away from the viewer. If
    vertex normals are assigned, they should point in the general
    direction of the surface normal, otherwise unpredictable results
    may occur.

    If a face has a texture map assigned to it and no texture vertices
    are assigned in the f statement, the texture map is ignored when
    the element is rendered.

> type Face = [References]
> faceElements :: OBJParser t Face
> faceElements = string "f " >> manyTill (spaces >> idxs <$> int `sepBy1` (char '/')) eol
>   where idxs = zipWith ($) [OBJVertexIndex, OBJTextureIndex, OBJNormalIndex]


Grouping
--------

There are four statements in the .obj file to help you manipulate groups
of elements:

- Gropu name statements are used to organize collections of
    elements and simplify data manipulation for operations in
    Model.


Syntax
~~~~~~

- g group_name1 group_name2 . . .

    Polygonal and free-form geometry statement.

    Specifies the group name for the elements that follow it. You can
    have multiple group names. If there are multiple groups on one
    line, the data that follows belong to all groups. Group information
    is optional.

    group_name is the name for the group. Letters, numbers, and
    combinations of letters and numbers are accepted for group names.
    The default group name is default.

> type Group = String
> group :: OBJParser t [Group]
> group = string "g " >> many1 (spaces >> identifier)

s group_number

    Polygonal and free-form geometry statement.

    Sets the smoothing group for the elements that follow it. If you do
    not want to use a smoothing group, specify off or a value of 0.

    To display with smooth shading in Model and PreView, you must
    create vertex normals after you have assigned the smoothing groups.
    You can create vertex normals with the vn statement or with the
    Model program.

    To smooth polygonal geometry for rendering with Image, it is
    sufficient to put elements in some smoothing group. However, vertex
    normals override smoothing information for Image.

    group_number is the smoothing group number. To turn off smoothing
    groups, use a value of 0 or off. Polygonal elements use group
    numbers to put elements in different smoothing groups. For
    free-form surfaces, smoothing groups are either turned on or off;
    there is no difference between values greater than 0.

> data GroupNumber = On Int | Off deriving ( Show )
> smoothingGroup :: Parser GroupNumber
> smoothingGroup = do
>   string "s "
>   onOrOff <$> int <|> (string "off" >> return Off)
>   where onOrOff 0 = Off
>         onOrOff i = On i

o object_name

    Polygonal and free-form geometry statement.

    Optional statement; it is not processed by any Wavefront programs.
    It specifies a user-defined object name for the elements defined
    after this statement.

    object_name is the user-defined object name. There is no default.

> type ObjectName = String
> objectName :: OBJParser t ObjectName
> objectName = string "o " >> identifier


https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec

Small atomic parser 

concat and prepending

> (<++>) a b = (++) <$> a <*> b
> (<:>) a b = (:) <$> a <*> b

> number = many1 digit

> positiveInt = char '+' *> number

> negativeInt = char '-' <:> number

> int :: Parsec ByteString u Int 
> int = fmap rd $ negativeInt <|> positiveInt <|> number
>   where rd = read :: String -> Int

> int' :: Parsec ByteString u String
> int' = negativeInt <|> positiveInt <|> number

> float :: Parsec ByteString u Float
> float = fmap rd $ int' <++> decimal <++> e
>     where rd       = read :: String -> Float
>           decimal  = option "" $ char '.' <:> number
>           e        = option "" $ oneOf "eE" <:> int'

> v3 :: Parsec ByteString u (V3 Float)
> v3 = V3 <$> (spaces >> float)
>         <*> (spaces >> float) 
>         <*> (spaces >> float)

> v2 :: Parsec ByteString u (V2 Float)
> v2 = V2 <$> (spaces >> float)
>         <*> (spaces >> float)

> identifier = many1 (alphaNum <|> char '_')


> blankline = eol <|> (spaces >> eol) <|> string "\0"


Comments

Comments can appear anywhere in an .obj file. They are used to annotate
the file; they are not processed.

Here is an example:

    # this is a comment

The Model program automatically inserts comments when it creates .obj
files. For example, it reports the number of geometric vertices,
texture vertices, and vertex normals in a file.

    # 4 vertices
    # 4 texture vertices
    # 4 normals

> type Comment = String 
> comment :: Parsec ByteString u Comment
> comment = char '#' >> manyTill anyChar eol

> eol :: Parsec ByteString u String
> eol = try (string "\n\r")
>   <|> try (string "\r\n")
>   <|> string "\n"
>   <|> string "\r"

> skipLine = manyTill anyChar eol >> return ()

> makeLenses ''OBJElements
> makeLenses ''OBJVertexData
> makeLenses ''OBJ


> parseOBJ :: OBJParser t OBJ
> parseOBJ = do
>   many1 (line)
>   eof
>   getState
>   where
>       line :: OBJParser t () 
>       line = try ( geovertex     >>= \v -> modifyState (vertexData.geometricVertices %~ (`snoc` v)) )
>          <|> try ( vertexnormal  >>= \n -> modifyState (vertexData.vertexNormals     %~ (`snoc` n)) )
>          <|> try ( texvertex     >>= \t -> modifyState (vertexData.textureVertices   %~ (`snoc` t)) )
>          <|> try ( faceElements  >>= \f -> modifyState (elements.faces               %~ (`snoc` f)) )
>          <|> try ( lineElements  >>= \l -> modifyState (elements.lines               %~ (`snoc` l)) )
>          <|> try ( pointElements >>= \p -> modifyState (elements.points              %~ (`snoc` p)) )
>          <|> try ( objectName    >>= \n -> modifyState (name ?~ n) )
>          <|> try ( comment       >>= \c -> modifyState (comments <>~ [c]) )
>          <|> (blankline >> return ())
>          <|> skipLine

> parseOBJFile :: FilePath -> IO OBJ
> parseOBJFile filepath = do
>   input <- readFile $ fpToString filepath
>   case runParser parseOBJ mempty (fpToString filepath) input of
>       Left err -> error $ show err
>       Right y  -> return y


> instance Monoid OBJElements where
>   mempty  = memptydefault
>   mappend = mappenddefault
> instance Monoid OBJVertexData where
>   mempty  = memptydefault
>   mappend = mappenddefault
> instance Monoid OBJ where
>   mempty  = memptydefault
>   mappend = mappenddefault

> instance NFData OBJIndex      where rnf = genericRnf
> instance NFData OBJVertexData where rnf = genericRnf
> instance NFData OBJElements   where rnf = genericRnf
> instance NFData OBJ           where rnf = genericRnf
