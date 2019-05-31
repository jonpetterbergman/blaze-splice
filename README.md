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

Runtime loading supports macros which have the type:
```haskell
data Macro e m =
  Macro (Map String (Macro e m) -> e -> Map String Html -> Html -> m Html)
```
Macros are parameterized over an environment e and a monad m. Macros expand to Html in m. They take a map of named arguments, and one unnamed argument. They also take an environment e and a set of macros.

Macros can be called in 3 ways from the xml:

### Inside parameter 
```xml
<tag param="$macroname">
  ``` 
will call macroname with no named parameters and empty unnamed parameter.
### As a tag, named arguments as parameters 
```xml
<macro:macroname p1="named argument 1" p2="named argument 2">unnamed argument</macro:macroname>
```
### As a tag, named arguments as child nodes 
```xml
<longmacro:macroname>
  <p1>named argument 1</p1>
  <p2>named argument 2</p2>
  <body>unnamed argument</body>
</longmacro:macroname>
  ```
