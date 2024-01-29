Demo at https://www.tristanpendergrass.com/elm-empty-project.

# Development

```
$ npm install
$ npm start
```

# Deployment

## Before first deploy
There's some steps to follow for the first deploy to modify from the elm-empty-project name to the new name:
* In `README.md` modify the url for the demo
* In `package.json` modify parcel:build to have the right url
* Remove first time deploy section ^

## Deploying to Github pages
* `$ npm run build` or `$ npm run build-mac`: This command builds files in the /docs directory by default
* Push built files to Github
* Log into Github on an account that can edit settings of your project
* Your repo -> Settings -> Pages -> Build and Deployment -> Branch -> master branch, /docs folder

# Todos
## MVP
* [x] Show shortcuts
* [x] Get art working
* [x] Do the rest of the art
* [ ] Show better win or fail message

## Bonus
* [ ] Make clicking whole shelf item buy it instead of buy button
* [ ] Give brand names some stylistic differentiation
* [ ] Make gift card look better
* [ ] Animation buying item
* [ ] Animate shelf changing