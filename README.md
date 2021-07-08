# HEB

### To Run

- run migrations
  - uncomment `main` code in `src/Script/Migration.hs`
  - run `stack runghc -- src/Script/Migration.hs`
  - recomment `main` code in `src/Script/Migration.hs`
- run application
  - run `stack run`

### API Summary

- POST /images
  - request body
    - should be formatted as valid JSON
    - _requires_
      - uri - String = the uri of your image e.g. "https://cat.jpg"
    - _optional parameter_
      - detectionEnable - Boolean = whether objectDetection should be run for the uri image
      - label - String = customized label for the image
    - examples
      ```
        {
        "label": "my-label",
        "detectionEnabled": false,
        "mime": "image/jpeg",
        "uri": "https://images.squarespace-cdn.com/content/v1/5a2c764af43b551b489c752d/1519106251480-41VEYAUTRH7T84N0HMH0/javacatscafe18Feb20180088-2.jpg"
      }
      ```
