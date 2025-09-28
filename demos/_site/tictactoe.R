init_board <- function() {
  matrix(" ", nrow = 3, ncol = 3)
}

print_board <- function(board) {
  paste(
    apply(board, 1, function(row) paste(row, collapse = " | ")),
    collapse = "\n---------\n"
  )
}

check_winner <- function(board) {
  lines <- rbind(
    board,
    t(board),
    c(board[1, 1], board[2, 2], board[3, 3]),
    c(board[1, 3], board[2, 2], board[3, 1])
  )
  for (i in 1:nrow(lines)) {
    line <- lines[i, ]
    if (all(line == "X")) return("X")
    if (all(line == "O")) return("O")
  }
  return(NA_character_)
}

is_board_full <- function(board) {
  !any(board == " ")
}

pos_to_coord <- function(pos) {
  if (pos < 1 || pos > 9) return(NULL)
  row <- (pos - 1) %/% 3 + 1
  col <- (pos - 1) %% 3 + 1
  c(row, col)
}

make_move <- function(board, pos, player) {
  coord <- pos_to_coord(pos)
  if (is.null(coord)) return(list(board = board, valid = FALSE))
  if (board[coord[1], coord[2]] != " ") return(list(board = board, valid = FALSE))
  board[coord[1], coord[2]] <- player
  list(board = board, valid = TRUE)
}

ai_move <- function(board) {
  empty_cells <- which(board == " ", arr.ind = TRUE)
  if (nrow(empty_cells) == 0) return(NULL)
  choice <- empty_cells[sample(nrow(empty_cells), 1), , drop = FALSE]
  pos <- (choice[1] - 1) * 3 + choice[2]
  as.numeric(pos)
}

play_turn <- function(board, player_move_pos) {
  res <- make_move(board, player_move_pos, "X")
  if (!res$valid) {
    return(list(
      board = board,
      board_text = print_board(board),
      message = "⚠️ Invalid move! Try again.",
      game_over = FALSE,
      winner = NA
    ))
  }
  board <- res$board
  winner <- check_winner(board)
  if (!is.na(winner)) {
    return(list(
      board = board,
      board_text = print_board(board),
      message = "🎉 You win!",
      game_over = TRUE,
      winner = winner
    ))
  }
  if (is_board_full(board)) {
    return(list(
      board = board,
      board_text = print_board(board),
      message = "It's a draw!",
      game_over = TRUE,
      winner = NA
    ))
  }
  ai_pos <- ai_move(board)
  if (!is.null(ai_pos)) {
    res <- make_move(board, ai_pos, "O")
    board <- res$board
  }
  winner <- check_winner(board)
  if (!is.na(winner)) {
    return(list(
      board = board,
      board_text = print_board(board),
      message = "💀 AI wins!",
      game_over = TRUE,
      winner = winner
    ))
  }
  if (is_board_full(board)) {
    return(list(
      board = board,
      board_text = print_board(board),
      message = "It's a draw!",
      game_over = TRUE,
      winner = NA
    ))
  }
  return(list(
    board = board,
    board_text = print_board(board),
    message = "Your turn! Enter 1–9:",
    game_over = FALSE,
    winner = NA
  ))
}

# Initialize global board
.global_board <- init_board()
