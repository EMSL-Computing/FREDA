# identify null elements or elements with more sub-elements than the specified threshold
null_or_maxlen <- function(x, maxlen) {
  is.null(x) | length(x) > maxlen | isTRUE(is.na(x))
}

# helper function passed to map().  Given a list column containing elements x, create a new list column with ALL elements of map_list[x]
newcol_from_mapping = function(x, maxlen, map_list) {
  if (!all(is.na(x))) {
    x <- x %>% unlist() %>% unique()
    temp <- if (is.character(map_list)) get(map_list)[x] else map_list[x]
    temp <- temp[which(lapply(temp, null_or_maxlen, maxlen = maxlen) == F)]
    if (length(temp) > 0) {
      return(temp)
    }
    else return(list(NA))
  }
  else return(x)
}

# function for un-listing elements of a particular column which contains lists (dict) which are named after elements of another column (keys)
unnest_by_key = function(keys, dict) {
  if (!all(is.na(dict))) {
    if (!all(is.na(keys))) {
      # the elements of the column will be lists with elements named after the specified keys
      foo <- dict[[keys]]
      if (length(foo) > 0) {
        return(foo)
      }
      else NA
    }
    else NA
  }
  else NA
}

list2semicolon <- function(x) {
  x %>%
    unlist() %>%
    unname() %>%
    unique() %>%
    paste(collapse = ';')
}

# from a list of [thing : elements_things_maps_to] key-value pairs create the reverse mapping
make_reverse_mapping <- function(mapping, key_name, value_name) {
  mapping %>%
    tibble::enframe(name = key_name, value = value_name) %>%
    tidyr::unnest() %>%
    group_by(!!rlang::sym(value_name)) %>%
    tidyr::nest(.key = !!rlang::sym(key_name)) %>%
    mutate(!!rlang::sym(key_name) := map(!!rlang::sym(key_name), function(x) unname(unlist))) %>%
    tibble::deframe()
}
