require(FNN)  # Fast nearby neighbour  using C++ implementation

## Find the nearest neighbour using a KD Tree
query <- matrix(rnorm(10), ncol = 2)
reference <- matrix(rnorm(10), ncol = 2)
Neighbour(query, reference, 3, "kdtree", 0, 1)


# use the R API CLIENT PACKAGE
require(rapiclient)

pet_api <- get_api(url = "http://petstore.swagger.io/v2/swagger.json")
operations <- get_operations(pet_api)
schemas <- get_schemas(pet_api)

res <- operations$getPetById(petId = 42)  # non existent
str(httr::content(res))

res <- 
  operations$addPet(
    id = 42,
    category = schemas$Category(
      id = 1,
      name = "Undefined"
    ),
    name = "Agrajag",
    photoUrls = list(),
    tags = list(
      schemas$Tag(id = 1, name = "Wild"),
      schemas$Tag(id = 2, name = "Furry")
    ),
    status = "available"
  )
res$status_code   # 200 is success

res <- operations$getPetById(petId = 42)  # should exist this time!
str(httr::content(res))

# NY TIMES API
nyt_api <- get_api("http://developer.nytimes.com/top_stories_v2.json/swagger.json")

nyt_operations <- 
  get_operations( nyt_api, .headers = c("api-key" = Sys.getenv("NYT_API_KEY")))

res <- nyt_operations$Top_Stories(section = "science", format = "json")

res$status_code