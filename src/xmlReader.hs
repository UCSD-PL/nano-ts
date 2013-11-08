import System.Environment
import System.IO
import Text.XML.Light

data MyTags = 
   SourceUnit [MyTags]
  |List  [MyTags]
  |ExpressionStatement [MyTags]
  |MultiplyExpression  [MyTags]
  |ParenthesizedExpression [MyTags]
  |OpenParenToken  
  |CloseParenToken  
  |AddExpression [MyTags]
  |NumericLiteral  
  |PlusToken  
  |AsteriskToken  
  |SemicolonToken 
  |EndOfFileToken 
  |Empty
  deriving (Show)


main = do (arg :_) <- getArgs
          contents <- readFile arg
          --let xml   = stringToXML contents
          --let out   = xmlToString xml
          case parseXMLDoc contents of -- returns a Maybe Element
                Nothing -> error "Failed to parse xml"
                Just rootEl  -> print (tagTextToHuman (qName(elName rootEl),rootEl))



createPairs :: [Content] -> [(String, Element)]
createPairs [] = []
createPairs l  = [ (qName (elName e), e) | x@(Elem e) <- l]

getText :: [Content] -> String
getText l  = concat [ (cdData t) | x@(Text t) <- l]

tagTextToMyTags :: (String,Element) -> MyTags
tagTextToMyTags ("SourceUnit",list) = SourceUnit (map tagTextToMyTags (createPairs (elContent list)))
tagTextToMyTags ("List",list) = List (map tagTextToMyTags (createPairs (elContent list)))
tagTextToMyTags ("ExpressionStatement",list) = ExpressionStatement (map tagTextToMyTags (createPairs (elContent list)))
tagTextToMyTags ("MultiplyExpression",list) = MultiplyExpression (map tagTextToMyTags (createPairs (elContent list)))
tagTextToMyTags ("ParenthesizedExpression",list) = ParenthesizedExpression (map tagTextToMyTags (createPairs (elContent list)))
tagTextToMyTags ("OpenParenToken",list) = OpenParenToken
tagTextToMyTags ("CloseParenToken",list) = CloseParenToken
tagTextToMyTags ("AddExpression",list) = AddExpression (map tagTextToMyTags (createPairs (elContent list)))
tagTextToMyTags ("NumericLiteral",list) = NumericLiteral
tagTextToMyTags ("PlusToken",list) = PlusToken
tagTextToMyTags ("AsteriskToken",list) = AsteriskToken
tagTextToMyTags ("SemicolonToken",list) = SemicolonToken
tagTextToMyTags ("EndOfFileToken",list) = EndOfFileToken
tagTextToMyTags (s,list) = error ("Exception: string of type:" ++ s)

tagTextToHuman :: (String,Element) -> String
tagTextToHuman ("SourceUnit",list) = concat (map tagTextToHuman (createPairs (elContent list)))
tagTextToHuman ("List",list) =  concat (map tagTextToHuman (createPairs (elContent list)))
tagTextToHuman ("ExpressionStatement",list) =  concat (map tagTextToHuman (createPairs (elContent list)))
tagTextToHuman ("MultiplyExpression",list) =  concat (map tagTextToHuman (createPairs (elContent list)))
tagTextToHuman ("ParenthesizedExpression",list) =  concat (map tagTextToHuman (createPairs (elContent list)))
tagTextToHuman ("OpenParenToken",list) = "(" 
tagTextToHuman ("CloseParenToken",list) = ")"
tagTextToHuman ("AddExpression",list) =  concat (map tagTextToHuman (createPairs (elContent list)))
tagTextToHuman ("NumericLiteral",list) = getText (elContent list)
tagTextToHuman ("PlusToken",list) = "+"
tagTextToHuman ("AsteriskToken",list) = "*"
tagTextToHuman ("SemicolonToken",list) = ";"
tagTextToHuman ("EndOfFileToken",list) = ""
tagTextToHuman (s,list) = error ("Exception: string of type:" ++ s)



--data Expr = 


--main = do
 --       s <- readFile "rss.xml"
   --     case parseXMLDoc s of
     --     Nothing  -> error "Failed to parse xml"
       --   Just doc -> let v1 = showTopElement doc
         ---                 Just doc2 = parseXMLDoc v1
            --              v2 = showTopElement doc2
              --        in print (v1 == v2)