### To Run

- Create database
  - create database in postgresql named "heb"
    - from command line: `createdb heb`
- Run migrations
  - uncomment `main` code in `src/Database/Migration.hs`
  - run `stack runghc -- src/Database/Migration.hs`
  - recomment `main` code in `src/Database/Migration.hs`
- Run application
  - run `stack run`
  - The web application will begin to run on `localhost:3000`

### API Summary

- `POST /images`
  - Request body
    - Should be formatted as valid JSON
    - _Requires_
      - uri :: String = the uri of your image e.g. "https://cat.jpg"
    - _Optional parameters_
      - detectionEnable :: Bool = whether objectDetection should be run for the uri image
        - if parameter is not provided or is set to `false` the 'detectedObjects` field on
          the created image record will not be populated
      - label :: String = customized label for the image
        - if not provided the image label will default to the image's uuid
    - Error handling
      - if an image is unabled to be saved a 500 status code will be returned
    - Example request bodies
      - With no options
      ```
      {
        "uri": "https://i.pinimg.com/originals/4a/4b/1a/4a4b1ac7f281f29c887f90b83787bb8b.jpg"
      }
      ```
      - With all options
      ```
      {
        "label": "my-label",
        "detectionEnabled": true,
        "uri": "https://i.pinimg.com/originals/4a/4b/1a/4a4b1ac7f281f29c887f90b83787bb8b.jpg"
      }
      ```
- `GET /images/:uuid`

  - Retrieves a saved image record using the image's uuid field
  - If the provided uuid does not correspond to a saved image record 404 status code will be returned

- `GET /images`
  - Returns all saved image records

### Service Design

- Endpoint handlers are defined in `src/Main.hs`
- Endpoint handlers primarily call into `Model.Image` and `Service.Image`
  - `Model.Image` contains db interactions with the underlying `model` db table
  - `Service.Image` orchestrates multiple image related requests
    - currently, this is limited to calling Imagga and attaching results to an image request-object
- Object detection executed by calling to [Imagga](https://imagga.com/) via `Client.Imagga`

### Project Notes

- Summary
  - I believe I was able to complete most of the project with the exception of the `GET /images` endpoint
    not being scopeable by object tag using `?objects=`. Although there looks to be [support for jsonb filtering](https://hackage.haskell.org/package/persistent-postgresql-2.13.0.3/docs/Database-Persist-Postgresql-JSON.html)
    I could not determine how to properly format a JSONB query filter using Persist / Esqueledo in the time permitted.
  - I am relatively new to Haskell so I'm sure most of my implementation is not idiomatic. I focused on implementing
    as much as I could while still keeping the code readable.
- Library Choices
  - I used `Persist` with `Postgres` because Persist seemed like a good option with good documentation and  
    Postgres has strong JSONB support which would help with the object tag search.
  - I used `Scotty` because it was relatively simple and had good documentation.
  - I used `Imagga` because requests only required a valid api key contained in an authorization header. Other options
    like Google Cloud Vision and Amazon Rekognition required a large amount of initial setup.
- Next on Todo List
  - Solve JSONB query problem, scope index by detected object tags query param
  - Add endpoint tests that validate request and response structures, especially failure scenarios
  - Come up with better approach for running migrations
