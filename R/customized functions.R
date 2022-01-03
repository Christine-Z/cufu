# -----------------------------------------------------------
# customized functions
# -----------------------------------------------------------
# -----------------------------------------------------------
# building the frequency lists
# -----------------------------------------------------------

cufu_freqlist <- function(X_fnames, stop_list) {
  # build frequency list for target corpus
  # [use whitespace as token splitter]
  # [drop tokens containing ":", "[", or "]" ]
  X_flist <- X_fnames %>%
    freqlist(re_token_splitter = r"--[(?xi)  \s+   ]--",
             re_drop_token     = r"--[(?xi)  [:\[\]] ]--",
             file_encoding     = "UTF-8") 
  # inspecting the top freq. items (after dropping stop words)
  # [resorting to concordances when the need arises]
  X_flist <- X_flist %>% 
    drop_types(stop_list)
  # return list
  return(X_flist)
}

# ------------------------------------------------------------------
# Correspondence analysis
# ------------------------------------------------------------------

# -------------------------------------------------------------------
# CA: function words as features
# -------------------------------------------------------------------
cufu_funword <- function(feature,fnames,short_fnames){

  # build a data.frame df with in its row names the features  
  df <- data.frame(row.names = feature)
  
  for (i in 1:length(fnames)) {
    fname <- fnames[[i]]             # identify i-th filename
    short_fname <- short_fnames[[i]] # identify i-th short filename
    flist <- freqlist(fname)         # build frequency list for file
    flist <- flist[feature]         # filter that list to just features
    df[[short_fname]] <- flist        # add column to d named after filename
  }
  
  df <- df %>%
    as.matrix() %>%
    t()%>%
    drop_empty_rc()
  
  return(df)
}

# -------------------------------------------------------------------
# CA: plotting
# -------------------------------------------------------------------

cufu_textcoord <- function(A_ca, B_ca){
  
  text_coord_A <- row_pcoord(A_ca)                # coordinates of texts
  text_coord_B <- row_pcoord(B_ca)
  
  A_texts_df <- tibble(
    text = A_short_fnames,
    sub_corp = 'First Decade',
    x = text_coord_A[, 1],
    y = text_coord_A[, 2])
  
  B_texts_df <- tibble(
    text = B_short_fnames,
    sub_corp = 'Second Decade',
    x = text_coord_B[, 1],
    y = text_coord_B[, 2])
  
  texts_df <- rbind(A_texts_df, B_texts_df)
  
  return(texts_df)
}
  
cufu_wordcoord <- function(A_ca, B_ca, d_A, d_B){ 
  
  A_word_coord <- col_pcoord(A_ca)                # coordinates of function words
  B_word_coord <- col_pcoord(B_ca) 
  
  A_words_df <- tibble(
    word = colnames(d_A),
    x = A_word_coord[, 1],
    y = A_word_coord[, 2])
  
  B_words_df <- tibble(
    word = colnames(d_B),
    x = B_word_coord[, 1],
    y = B_word_coord[, 2])
  
  words_df <- rbind(A_words_df, B_words_df)
  
  return(words_df)

}
