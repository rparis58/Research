# tibble:-   data_frame
# options(tibble.width = Inf)

require(tibble)
#vignette(tibble)

as_data_frame(iris)
data_frame(x=1:5, y=1, z=x^2 + y)

# define a data_frame row by row
tibble::frame_data(
  ~colA, ~colB, ~colZ,
  "a",   1, 11,
  "b",   2, 12
)

