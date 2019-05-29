# blaze-splice
Splice Html into your blaze-html at runtime or compiletime.
## Intro
blaze-html is great for generating html, but sometimes you have a large chunk of html from elsewhere that you want to include, say an svg you made using a drawing application. With blaze-splice you can paste it right in:
```haskell
{-# language QuasiQuotations #-}
import Text.Blaze.Html(Html)
import Text.Blaze.Splice(xml)

mySvg :: Html
mySvg = 
  [xml|
    <svg viewBox="-5 -5 10 10">
      <g transform="rotate(-45)">
        <path d="m -3 0 a 1 1 0 0 1 1 -1 l 4 0 a 1 1 0 0 1 1 1" 
              stroke="black" fill="none" 
              stroke-width="1.2" 
              stroke-linejoin="round">
        </path>
        <rect x="-4" y="-0.1" width="2" height="0.8" rx="0.2" ry="0.2"></rect>
        <rect x="2" y="-0.1" width="2" height="0.8" rx="0.2" ry="0.2"></rect>
      </g>
    </svg>
  |]
```

It might be more conveninent to load the svg (at compile time) from a separate file in case you want to change it in the future:

```haskell
import Text.Blaze.Splice(xmlFile)

mySvg = [xmlFile|svg/phone.svg|]
```
(you need to include such files in your .cabal's "extra-source-files" section)

## Variable interpolation

You might have html code with headers and footers, where some dynamic content goes in the middle:

```haskell
myTemplate :: Html -> Html
myTemplate content = 
  [xml|
    <html>
      <head><title>MyPage</title></head>
      <body>
        $content
      </body>
    </html>
  |]
```

Interpolation is also supported in attributes:
```haskell
myTemplate :: AttributeValue -> Html -> Html
myTemplate lang content = 
  [xml|
    <html lang="$lang">
      <head><title>MyPage</title></head>
      <body>
        $content
      </body>
    </html>
  |]
```
## Runtime loading
Sometimes it's better to load certain content at runtime:
```haskell
import Text.Blaze.Load(loadFile)

loadContent :: IO Html
loadContent = loadFile mempty () "myContent.html"
```

Runtime loading has a sort of macro support. TODO: describe macros
