array_of_double = c(104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100) # Since R is for statistics, it generally defaults everything to floating-point types
array_of_ints = as.integer(array_of_double) # but you can deal with integers if you want.
print(array_of_ints)

first_element_of_array = array_of_ints[1] # indexing in R starts at 1
print(first_element_of_array)

string = intToUtf8(array_of_ints) # many functions in R operate on arrays or on individuals.
print(string)
