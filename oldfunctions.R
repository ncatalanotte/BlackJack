initData <- function(datatype) {
  if(datatype == "appdata") {
    appdata <- reactiveValues(deck = shuffle_deck(create_deck()),
                              count = 0,
                              number_of_cards = 52,
                              recentcard = NULL,
                              gamefinished = FALSE,
                              reshuffle = FALSE
    )} else if (datatype == "dealer") {
      dealer <- reactiveValues(firstcard = NULL,
                               secondcard = NULL,
                               showsecondcard = FALSE,
                               hitcards = list(),
                               is21 = NULL,
                               newcards = FALSE)}
  else if (datatype == "player") {
    player <- reactiveValues(buyin = 0,
                             bet = 0,
                             matchbet = 0,
                             firstcard = NULL,
                             secondcard = NULL,
                             hitcards = list(),
                             total = NULL,
                             is21 = NULL,
                             match = FALSE,
                             royalmatch = FALSE,
                             rematch = FALSE,
                             recentgameresult = NULL,
                             newcards = FALSE)}
  else {}
}

create_deck <- function() {
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  icons <- c("heart", "diamond", "club", "spade")
  values <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")
  deck <- data.frame(
    Value = rep(values, each = length(suits)),
    Suit = rep(suits, times = length(values)),
    Icon = rep(icons, times = length(values))
  )
  return(deck)
}

shuffle_deck <- function(deck) {
  shuffled_deck <- deck[sample(1:nrow(deck), size = nrow(deck)), ]
  return(shuffled_deck)
}

get_card_value <- function(card) {
  if (card$Value %in% c("Jack", "Queen", "King")) {
    return(10)
  } else if (card$Value == "Ace") {
    return(11)  # You will need to handle the Ace logic in gameplay functions
  } else {
    return(as.numeric(card$Value))
  }
}

deal_cards <- function(deck, num_cards) {
  
  dealt_cards <- deck[1:num_cards, ]
  remaining_deck <- deck[-(1:num_cards), ]
  return(list("cards" = dealt_cards, "deck" = remaining_deck))
}

fulldeal <- function(appdata) {
  
  appdata$recentcard <- deal_cards(appdata$deck, 1)$cards
  appdata$recentcard$number_value <- get_card_value(appdata$recentcard)
  remainingdeck <- deal_cards(appdata$deck, 1)$deck
  
  appdata$deck <- remainingdeck
  
  if (appdata$recentcard$number_value >= 2 && appdata$recentcard$number_value <= 6) {
    appdata$count <- appdata$count + 1
  } else if (appdata$recentcard$Value %in% c("Jack", "Queen", "King", "Ace", "10")) {
    appdata$count <- appdata$count - 1
  }
}

apptheme <- function() {
  bs_theme(
    bg = "#0D6220",    # Classic casino table green
    fg = "#DAA520",    # Dark wood tone
    primary = "#C8102E", # Vibrant red, typical of playing card red
    secondary = "#3D2B1F", # Gold color for secondary accents
    success = "#4CAF50",  # Softer green for successful actions
    info = "#2196F3",    # Bright blue for informational alerts
    warning = "#FFC107", # Amber for warnings
    danger = "#D32F2F",  # Deeper red for danger alerts
    
  )
}

cardcolor <- function(suit) {
  
  if(suit %in% c("Hearts", "Diamonds")) {
    'background-color: #f7f7f7!important; color: #C8102E;'
  } else {
    'background-color: #f7f7f7!important; color: #000000;'
  }
  
  
}

clearGame <- function(appdata, dealer, player, output) {
  appdata$recentcard <- NULL
  appdata$gamefinished <- FALSE
  dealer$firstcard <- NULL
  dealer$secondcard <- NULL
  dealer$showsecondcard <- FALSE
  dealer$hitcards <- list()
  dealer$is21 <- NULL
  dealer$newcards <- FALSE
  player$firstcard <- NULL
  player$secondcard <- NULL
  player$hitcards <- list()
  player$total <- NULL
  player$is21 <- NULL
  player$recentgameresult <- NULL
  player$newcards <- FALSE
  player$match <- FALSE
  player$royalmatch <- FALSE
  output$playertotalcards <- renderUI(NULL)
  output$dealertotalcards <- renderUI(NULL)
}

reshuffleCards <- function(appdata) {
  if(appdata$reshuffle == TRUE) {
    showNotification("Reshuffling Deck", duration = 15)
    appdata$deck <- shuffle_deck(create_deck()) 
    appdata$reshuffle = FALSE 
  } else {}
}

updateDeckCount <- function(appdata) {
  appdata$number_of_cards <- nrow(appdata$deck)
  if(appdata$number_of_cards <= 13) {appdata$reshuffle <- TRUE} else {appdata$reshuffle <- FALSE}
}

determineGameResult <- function(player, dealer) {
  if (player$total > 21) {
    player$recentgameresult <- "loss"
  } else if (dealer$total > 21 || player$total > dealer$total) {
    player$recentgameresult <- "normalwin"
  } else if (player$total == dealer$total) {
    player$recentgameresult <- "tie"
  } else {
    player$recentgameresult <- "loss"
  }
  if(player$recentgameresult == "normalwin") {
    player$buyin <- player$buyin + player$bet
  } else if (player$recentgameresult == "loss") {
    player$buyin <- player$buyin - player$bet
  } else {} 
}

finishDealerHand <- function(dealer, appdata) {
  if(dealer$total < 17) {
    fulldeal(appdata)
    dealer$hitcards <- append(dealer$hitcards, list(appdata$recentcard))
    new_card_value <- get_card_value(appdata$recentcard)
    dealer$total <- dealer$total + new_card_value
  }
  if(dealer$total < 17) {
    fulldeal(appdata)
    dealer$hitcards <- append(dealer$hitcards, list(appdata$recentcard))
    new_card_value <- get_card_value(appdata$recentcard)
    dealer$total <- dealer$total + new_card_value
  }
  if(dealer$total < 17) {
    fulldeal(appdata)
    dealer$hitcards <- append(dealer$hitcards, list(appdata$recentcard))
    new_card_value <- get_card_value(appdata$recentcard)
    dealer$total <- dealer$total + new_card_value
  }
}

playerHit <- function(appdata, player) {
  fulldeal(appdata)
  player$hitcards <- append(player$hitcards, list(appdata$recentcard))
  player$rematch <- if(player$secondcard$Suit == player$hitcards[[1]]$Suit && player$firstcard$Suit == player$hitcards[[1]]$Suit) {TRUE} else {FALSE} 
  new_card_value <- get_card_value(appdata$recentcard)
  player$total <- player$total + new_card_value
}

checkRematch <- function(player, input) {
  
  if(player$rematch == TRUE && player$match == TRUE && is.null(input$rematchbet) == FALSE) {
    print("player rematch")
    shinyalert::shinyalert(
      title = "Re Match!",
      text = "you re-matched your hit card, pays 3-1",
      type = "success"
    )
    player$buyin <- (player$buyin + (player$matchbet * 3)) 
  } else if (player$rematch == FALSE && is.null(input$rematchbet) == FALSE) { player$buyin <- (player$buyin - player$matchbet) }
  
}

checkBust <- function(player, appdata) {
  if (player$total > 21) { 
    print('checking for bust')
    shinyalert::shinyalert(
      title = "Bust!",
      text = "you busted",
      type = "warning"
    )
    output$actioncontrols <- renderUI(NULL)
    player$buyin <- (player$buyin - player$bet)
    updateDeckCount(appdata)
  } else {}
}

noBuyinModal <- function() {
  shinyalert::shinyalert(
    title = "Error!",
    text = "No buy in detected, please buy into game to begin",
    type = "error"
  )
}

setBet <- function (player, input, dealer) {
  player$bet <- input$playerbetamount
  player$matchbet <- input$playermatchbet
  player$newcards <- TRUE
  dealer$newcards <- TRUE
}

dealGame <- function(appdata, player, dealer) {
  fulldeal(appdata)
  burncard <- appdata$recentcard
  fulldeal(appdata)
  player$firstcard <- appdata$recentcard
  fulldeal(appdata)
  dealer$firstcard <- appdata$recentcard
  fulldeal(appdata)
  player$secondcard <- appdata$recentcard
  player$total <- sum(player$firstcard$number_value + player$secondcard$number_value)
  player$is21 <- if(player$total == 21) {"Yes"} else {"No"}
  player$match <- if(player$firstcard$Suit == player$secondcard$Suit) {TRUE} else {FALSE} 
  player$royalmatch <- if (
    (player$firstcard$Suit == player$secondcard$Suit) &&
    (player$firstcard$Value == "King" || player$firstcard$Value == "Queen") &&
    (player$secondcard$Value == "King" || player$secondcard$Value == "Queen") ) {TRUE} else {FALSE}
  fulldeal(appdata)
  dealer$secondcard <- appdata$recentcard
  dealer$total <- sum(dealer$firstcard$number_value + dealer$secondcard$number_value)
  dealer$is21 <- if(dealer$total == 21) {"Yes"} else {"No"}
}

checkMatch <- function(player, output) {
  if(player$match == TRUE) {
    matchModal()
    output$rematch <- renderUI(checkboxInput("rematchbet", "Bet Re-Match"))
    player$buyin <- (player$buyin + (player$matchbet * 3))
  } else {
    player$buyin <- (player$buyin - player$matchbet)
    output$rematch <- renderUI(NULL)
  }
}

matchModal <- function() {
  shinyalert::shinyalert(
    title = "You Matched!",
    text = "First 2 Suits Match, pays 3 : 1",
    type = "success"
  )
}

checkRoyalMatch <- function(player) {
  if(player$royalmatch == TRUE) {
    royalMatchModal()
    player$buyin <- (player$buyin + (player$matchbet * 10))
  } else {}
}

royalMatchModal <- function() {
  shinyalert::shinyalert(
    title = "You Royal Matched!",
    text = "First 2 Suits King or Qeen's Match, pays 10 : 1",
    type = "success"
  )
}

check21 <- function(dealer, player) {
  if(dealer$is21 == "Yes") {
    dealerBlackjackModal()
    if(player$total == 21) {
      player$recentgameresult <- "tie"
    } else {
      player$recentgameresult <- "loss"
      player$buyin <- (player$buyin - player$bet)
    }
  } else if (player$is21 == "Yes") {
    player$buyin <- (player$buyin + (player$bet * 3/2))
    playerBlackjackModal()
  } else {}
}

dealerBlackjackModal <- function() {
  shinyalert::shinyalert(
    title = "Dealer Blackjack!",
    text = "dealer recieved blackjack, game over",
    type = "warning"
  )
}

playerBlackjackModal <- function() {
  shinyalert::shinyalert(
    title = "Blackjack!",
    text = "you recieved blackjack, pays out 3 to 2",
    type = "success"
  )
}

outputControls <- function(dealer, player, output) {
  output$actioncontrols <- renderUI({
    if (!(dealer$is21 == "Yes" || player$is21 == "Yes")) {
      layout_columns(
        actionButton("playerhit", "HIT"),
        actionButton("playerdouble", "DOUBLE"),
        actionButton("playerstand", "STAND"),
        col_widths = 4
      )
    } else {NULL}
  })
}

buyinGame <- function(player, input) {
  showModal(
    modalDialog(
      numericInput("playerdollars", "Buy in Amount", value = 500),
      actionButton("buyinplayer", "Buy into Game")
    )
  )
  observeEvent(input$buyinplayer, {
    player$buyin <- input$playerdollars
    removeModal()
  })
  
}

dealerCardUI <- function(dealer) {
  all_cards_ui <- c()
  if (!is.null(dealer$secondcard)) {
    first_card_ui <- list(
      bslib::value_box(
        style = cardcolor(dealer$firstcard$Suit),
        title = dealer$firstcard$Suit,
        value = dealer$firstcard$Value,
        showcase = bsicons::bs_icon(paste0("suit-", dealer$firstcard$Icon, "-fill")),
        style = "display: inline-block; margin-right: 10px;"
      )
    )
    all_cards_ui <- c(first_card_ui)
    if(dealer$showsecondcard == TRUE) {
      second_card_ui <- list(
        bslib::value_box(
          style = cardcolor(dealer$secondcard$Suit),
          title = dealer$secondcard$Suit,
          value = dealer$secondcard$Value,
          showcase = bsicons::bs_icon(paste0("suit-", dealer$secondcard$Icon, "-fill")),
          style = "display: inline-block; margin-right: 10px;"
        )
      )
      total_ui <- list(
        bslib::value_box(
          style = 'background-color: #DAA520!important; color: #0D6220;',
          title = "Dealer Total",
          value = dealer$total
        )
      )
      all_cards_ui <- c(first_card_ui, second_card_ui, total_ui)
    }
    if (length(dealer$hitcards) > 0) {
      hit_cards_ui <- lapply(dealer$hitcards, function(card) {
        bslib::value_box(
          style = cardcolor(card$Suit),
          title = card$Suit,
          value = card$Value,
          showcase = bsicons::bs_icon(paste0("suit-", card$Icon, "-fill")),
          style = "display: inline-block; margin-right: 10px;"
        )
      })
      all_cards_ui <- c(first_card_ui, second_card_ui, hit_cards_ui, total_ui)
    }
  }
  div(style = "display: flex;", all_cards_ui)
}

playerCardUI <- function(player, input) {
  bet_ui <- list(
    bslib::value_box(
      style = 'background-color: #DAA520!important; color: #0D6220;',
      title = "Bet",
      value = input$playerbetamount,
      showcase = bsicons::bs_icon("coin"),
      style = "display: inline-block; margin-right: 10px;"
    )
  )
  if (!is.null(player$secondcard)) {
    playing_cards_ui <- list(
      bslib::value_box(
        style = cardcolor(player$firstcard$Suit),
        title = player$firstcard$Suit,
        value = player$firstcard$Value,
        showcase = bsicons::bs_icon(paste0("suit-", player$firstcard$Icon, "-fill")),
        style = "display: inline-block; margin-right: 10px;"
      ),
      bslib::value_box(
        style = cardcolor(player$secondcard$Suit),
        title = player$secondcard$Suit,
        value = player$secondcard$Value,
        showcase = bsicons::bs_icon(paste0("suit-", player$secondcard$Icon, "-fill")),
        style = "display: inline-block; margin-right: 10px;"
      )
    )
    all_cards_ui <- c(bet_ui, playing_cards_ui)
    total_ui <- list(
      bslib::value_box(
        style = 'background-color: #DAA520!important; color: #0D6220;',
        title = "Total",
        value = player$total,
        style = "display: inline-block; margin-right: 10px;"
      )
    )
    all_cards_ui <- c(bet_ui, playing_cards_ui, total_ui)
    if (length(player$hitcards) > 0) {
      hit_cards_ui <- lapply(player$hitcards, function(card) {
        bslib::value_box(
          style = cardcolor(card$Suit),
          title = card$Suit,
          value = card$Value,
          showcase = bsicons::bs_icon(paste0("suit-", card$Icon, "-fill")),
          style = "display: inline-block; margin-right: 10px;"
        )
      })
      all_cards_ui <- c(bet_ui, playing_cards_ui, hit_cards_ui, total_ui)
    }
    div(style = "display: flex;", all_cards_ui)
  }
}

buyinUI <- function(player) {
  div(id="bankroll",
      bslib::value_box(
        style = 'background-color: #DAA520!important; color: #0D6220;',
        title = "Current Bankroll",
        value = player$buyin,
        showcase = bsicons::bs_icon("currency-dollar")
      )
  )
}

playerControlUI <- function() {
  div(class = "row",
      div(class = "col-sm", 
          uiOutput("playercurrentbuy")),
      div(class = "col-sm",
          numericInput("playerbetamount", "Bet Amount", value = 5),
          numericInput("playermatchbet", "Royal Match", value = 1),
          uiOutput("rematch")),
      div(class = "col-sm",
          layout_columns(
            actionButton("start", "start game"),
            actionButton("restart","clear cards"),
            width = 6
          ),
          uiOutput("actioncontrols")
      )
  )
}

dealerUI <- function() {
  bslib::card(
    bslib::card_header("Dealer"),
    bslib::card_body(
      uiOutput("dealertotalcards"),
      uiOutput("dealer21"),
      actionButton("settings", "Settings"))
  )
}

tableUI <- function() {
  bslib::card(
    bslib::card_header("Table"),
    bslib::card_body(
      uiOutput("playertotalcards")
    )
  )
}

playerUI <- function() {
  bslib::card(
    bslib::card_header("Player"),
    card_body(
      playerControlUI()
    )
  )
}
