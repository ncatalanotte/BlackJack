
#updated for split
    #low level functions 
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
    'background-color: #f7f7f7!important; color: #C8102E; width:100px;'
  } else {
    'background-color: #f7f7f7!important; color: #000000; width:100px;'
  }}
    #modals
      noBuyinModal <- function() {
        shinyalert::shinyalert(
          title = "Error!",
          text = "No buy in detected, please buy into game to begin",
          type = "error"
        )
      }
      matchModal <- function() {
        shinyalert::shinyalert(
          title = "You Matched!",
          text = "First 2 Suits Match, pays 3 : 1",
          type = "success"
        )
      }
      royalMatchModal <- function() {
        shinyalert::shinyalert(
          title = "You Royal Matched!",
          text = "First 2 Suits King or Qeen's Match, pays 10 : 1",
          type = "success"
        )
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
    #functions initalized on load of app
      initData <- function(datatype) {
        if (datatype == "appdata") {
          appdata <- reactiveValues(
            deck = shuffle_deck(create_deck()),
            count = 0,
            number_of_cards = 52,
            recentcard = NULL,
            gamefinished = FALSE,
            reshuffle = FALSE
          )
          return(appdata)  # Explicit return
          
        } else if (datatype == "dealer") {
          dealer <- reactiveValues(
            firstcard = NULL,
            secondcard = NULL,
            showsecondcard = FALSE,
            hitcards = list(),
            is21 = NULL,
            newcards = FALSE
          )
          return(dealer)  # Explicit return
          
        } else if (datatype == "player") {
          player <- reactiveValues(
            buyin = 0,
            hands = list(
              list(
                bet = 0,
                matchbet = 0,
                cards = list(),
                total = NULL,
                is21 = NULL,
                match = FALSE,
                royalmatch = FALSE,
                rematch = FALSE
              )
            ),
            recentgameresult = NULL,
            newcards = FALSE
          )
          return(player)  # Explicit return
          
        } else {
          stop("Invalid datatype specified for initData")
        }
      }
    #background game actions 
      updateDeckCount <- function(appdata) {
        appdata$number_of_cards <- nrow(appdata$deck)
        if(appdata$number_of_cards <= 13) {appdata$reshuffle <- TRUE} else {appdata$reshuffle <- FALSE}
      }
      reshuffleCards <- function(appdata) {
        if(appdata$reshuffle == TRUE) {
          showNotification("Reshuffling Deck", duration = 15)
          appdata$deck <- shuffle_deck(create_deck()) 
          appdata$count <- 0
          appdata$reshuffle = FALSE 
        } else {}
      }
      determineGameResult <- function(player, dealer) {
        # Iterate over each hand to determine results
        for (i in seq_along(player$hands)) {
          hand <- player$hands[[i]]
          
          if (hand$total > 21) {
            hand$result <- "loss"
          } else if (dealer$total > 21 || hand$total > dealer$total) {
            hand$result <- "normalwin"
          } else if (hand$total == dealer$total) {
            hand$result <- "tie"
          } else {
            hand$result <- "loss"
          }
          
          # Update the buyin based on the result of each hand
          if (hand$result == "normalwin") {
            player$buyin <- player$buyin + hand$bet
          } else if (hand$result == "loss") {
            player$buyin <- player$buyin - hand$bet
          }
          player$hands[[i]] <- hand
        }
      }
      checkMatch <- function(player, hand_index, output) {
        hand <- player$hands[[hand_index]]
        if (hand$match == TRUE) {
          matchModal()
          output[[paste("rematch", hand_index, sep = "")]] <- renderUI({
            checkboxInput("rematchbet", "Bet Re-Match", value = FALSE)
          })
          player$buyin <- (player$buyin + (hand$matchbet * 3))
        } else {
          player$buyin <- (player$buyin - hand$matchbet)
          output[[paste("rematch", hand_index, sep = "")]] <- renderUI(NULL)
        }
      }
      checkRoyalMatch <- function(player, hand_index) {
        hand <- player$hands[[hand_index]]
        
        if (hand$royalmatch == TRUE) {
          royalMatchModal()
          player$buyin <- (player$buyin + (hand$matchbet * 10))
        }
      }
      checkRematch <- function(player, hand_index, input) {
        # Access the specific hand
        hand <- player$hands[[hand_index]]
        
        # Check for the rematch condition for this specific hand
        if(hand$rematch == TRUE && hand$match == TRUE && !is.null(input$rematchbet)) {
          print("player rematch")
          shinyalert::shinyalert(
            title = "Re Match!",
            text = "You re-matched your hit card, pays 3-1",
            type = "success"
          )
          player$buyin <- player$buyin + (hand$matchbet * 3)
        } else if (hand$rematch == FALSE && !is.null(input$rematchbet)) {
          player$buyin <- player$buyin - hand$matchbet
        }
      }
      check21 <- function(dealer, player) {
        # Check if dealer has Blackjack
        if(dealer$is21 == "Yes") {
          dealerBlackjackModal()
          
          # Check all player hands for response to dealer Blackjack
          for (i in seq_along(player$hands)) {
            hand <- player$hands[[i]]
            if (hand$total == 21) {
              hand$result <- "tie"
            } else {
              hand$result <- "loss"
              player$buyin <- (player$buyin - hand$bet)  # Deduct bet from player's buyin
            }
            player$hands[[i]] <- hand  # Update the hand back into player's hands list
          }
        } else {
          # Check each hand for player Blackjack
          for (i in seq_along(player$hands)) {
            hand <- player$hands[[i]]
            if (hand$total == 21) {
              hand$result <- "Blackjack"
              player$buyin <- (player$buyin + (hand$bet * 3/2))  # Payout for Blackjack
              playerBlackjackModal()  # This could be adjusted to be more specific
            }
            player$hands[[i]] <- hand  # Update the hand back into player's hands list
          }
        }
      }
      checkBust <- function(player, hand_index, appdata, output) {
        # Access the specific hand
        hand <- player$hands[[hand_index]]
        
        # Check if the total for this hand exceeds 21
        if (hand$total > 21) {
          print('checking for bust')
          shinyalert::shinyalert(
            title = "Bust!",
            text = "You busted with hand " + hand_index,
            type = "warning"
          )
          
          # Update UI controls to reflect the bust (e.g., disable actions for this hand)
          output$actioncontrols <- renderUI(NULL)  # You might need to be more specific based on hand
          
          # Update player's buyin based on the bet for this specific hand
          player$buyin <- player$buyin - hand$bet
          
          # Optionally update the deck count if that's part of your game logic
          updateDeckCount(appdata)
          
          # Mark the hand as finished or update its status
          hand$busted <- TRUE
          player$hands[[hand_index]] <- hand
        }
      }
      setBet <- function(player, hand_index, input) {
        # Access the specific hand
        hand <- player$hands[[hand_index]]
        
        # Set the bet for the specific hand
        hand$bet <- as.numeric(input$playerbetamount)
        hand$matchbet <- as.numeric(input$playermatchbet)
        
        # Update the hand back into the player's list of hands
        player$hands[[hand_index]] <- hand
        
        # Indicate new cards need to be dealt (if this part of your logic still applies)
        player$newcards <- TRUE 
        
        print(player$newcards)
      }
    #in-game actions
      dealGame <- function(appdata, player, dealer) {
          fulldeal(appdata)
          burncard <- appdata$recentcard # burn card
  
          fulldeal(appdata)
          player$hands[[1]]$cards[[1]] <- appdata$recentcard # player first card
  
          fulldeal(appdata)
          dealer$firstcard <- appdata$recentcard # dealer first card
  
          fulldeal(appdata)
          player$hands[[1]]$cards[[2]] <- appdata$recentcard # player second card
  
          if (player$hands[[1]]$cards[[1]]$Value == player$hands[[1]]$cards[[2]]$Value) { 
          player$canSplit <- TRUE   # checking for split eligibility
           } 
          player$hands[[1]]$total <- sum(sapply(player$hands[[1]]$cards, function(card) get_card_value(card))) # calculate total
           player$hands[[1]]$is21 <- if(player$hands[[1]]$total == 21) {"Yes"} else {"No"} # check for 21
           player$hands[[1]]$match <- if(player$hands[[1]]$cards[[1]]$Suit == player$hands[[1]]$cards[[2]]$Suit) {TRUE} else {FALSE} # check for match
           player$hands[[1]]$royalmatch <- if (
                                                (player$hands[[1]]$cards[[1]]$Suit == player$hands[[1]]$cards[[2]]$Suit) &&
                                                (player$hands[[1]]$cards[[1]]$Value %in% c("King", "Queen")) &&
                                                (player$hands[[1]]$cards[[2]]$Value %in% c("King", "Queen"))) {TRUE} else {FALSE} # check for royal match
  fulldeal(appdata)
  dealer$secondcard <- appdata$recentcard # dealer second card
  dealer$total <- (get_card_value(dealer$firstcard) + get_card_value(dealer$secondcard)) # calculate dealer total
  dealer$is21 <- if(dealer$total == 21) {"Yes"} else {"No"}
  
  }
      clearGame <- function(appdata, dealer, player, output) {
        appdata$recentcard <- NULL
        appdata$gamefinished <- FALSE
        appdata$count <- 0  # Reset count if you're counting cards
        
        # Reset dealer information
        dealer$firstcard <- NULL
        dealer$secondcard <- NULL
        dealer$showsecondcard <- FALSE
        dealer$hitcards <- list()
        dealer$is21 <- NULL
        dealer$newcards <- FALSE
        dealer$total <- NULL  # Ensure dealer's total is also reset
        
        # Reset player information
        player$hands <- list(
          list(  
            bet = 0,
            matchbet = 0,
            cards = list(),
            total = NULL,
            is21 = NULL,
            match = FALSE,
            royalmatch = FALSE,
            rematch = FALSE
          )
        )
        player$canSplit <- FALSE
        player$recentgameresult <- NULL
        player$newcards <- FALSE
        
        # Reset UI elements
        output$playertotalcards <- renderUI(NULL)
        output$dealertotalcards <- renderUI(NULL)
      }
      finishDealerHand <- function(dealer, appdata) {
        while (dealer$total < 17) {
          fulldeal(appdata)
          dealer$hitcards <- append(dealer$hitcards, list(appdata$recentcard))
          new_card_value <- get_card_value(appdata$recentcard)
          dealer$total <- dealer$total + new_card_value
        }
      }
      playerHit <- function(appdata, player, hand_index) {
        # Deal a card to a specific hand
        fulldeal(appdata)
        hand <- player$hands[[hand_index]]  # Access the specific hand
        hand$cards <- append(hand$cards, list(appdata$recentcard))  # Add the new card to the hand
        
        # Calculate the new card value and update the total
        new_card_value <- get_card_value(appdata$recentcard)
        hand$total <- hand$total + new_card_value
        
        # Adjust Ace values if necessary to avoid busting
        if (hand$total > 21 && any(sapply(hand$cards, function(card) card$Value == "Ace"))) {
          # Adjust each Ace from 11 to 1 until total is under 21 or no more Aces can be adjusted
          for (card in hand$cards) {
            if (hand$total > 21 && card$Value == "Ace" && card$usedAsEleven) {
              hand$total <- hand$total - 10
              card$usedAsEleven <- FALSE  # Mark Ace as used as 1
            }
          }
        }
        
        # Check for rematch (need clear criteria here, placeholder for your logic)
        hand$rematch <- if(length(hand$cards) > 1 && all(sapply(hand$cards[-1], function(c) c$Suit == hand$cards[[1]]$Suit))) {
          TRUE
        } else {
          FALSE
        }
        
        # Update the hand in the player's list of hands
        player$hands[[hand_index]] <- hand
      }
      playerDouble <- function(appdata, player, hand_index, dealer, output) {
        # Access the specific hand
        hand <- player$hands[[hand_index]]
        
        # Double the bet for the specific hand
        hand$bet <- hand$bet * 2
        
        # Deal one card to this hand and end the action for this hand
        fulldeal(appdata)
        hand$cards <- append(hand$cards, list(appdata$recentcard))
        
        # Calculate the new card value and update the total
        new_card_value <- get_card_value(appdata$recentcard)
        hand$total <- hand$total + new_card_value
        
        # Check if this single new card results in a rematch (logic might need adjustment based on actual rematch rules)
        hand$rematch <- if(length(hand$cards) > 1 && all(sapply(hand$cards[-1], function(c) c$Suit == hand$cards[[1]]$Suit))) {
          TRUE
        } else {
          FALSE
        }
        
        # Update the hand back into the player's list of hands
        player$hands[[hand_index]] <- hand
        
        # No further action allowed after double, render UI accordingly
        output$actioncontrols <- renderUI(NULL)
        
        # Dealer turns over second card and plays out hand if not already done
        dealer$showsecondcard <- TRUE
        if (!dealer$gamefinished) {
          finishDealerHand(dealer, appdata)
          dealer$gamefinished <- TRUE
        }
        
        # Determine the result for all hands, might need adjustment to apply to specific hands
        determineGameResult(player, dealer)
        updateDeckCount(appdata)
      }
    #in-game UI functions 
      dealerUI <- function() {
        bslib::card(
          bslib::card_header("Dealer"),
          bslib::card_body(
            uiOutput("dealertotalcards"),
            uiOutput("dealer21"),
            actionButton("settings", "Settings"))
        )
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
      
      playerUI <- function() {
        bslib::card(
          bslib::card_header("Player"),
          card_body(
            playerControlUI()
          )
        )
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
                  actionButton("start", "Start Game"),
                  actionButton("restart", "Clear Cards"),
                  width = 6
                ),
                uiOutput("actioncontrols")
            )
        )
      }
        outputControls <- function(dealer, player, output) {
        output$actioncontrols <- renderUI({
          # print("[15] Generating UI controls")  # Debug statement
          
          # Initialize a list to hold UI elements for all hands
          ui_elements <- list()
          
          # Loop through each hand to create controls
          for (i in seq_along(player$hands)) {
            hand <- player$hands[[i]]
            # print(paste("Hand", i, "is21:", hand$is21))
            
            if (!isTRUE(hand$is21)) {
              hand_controls <- layout_columns(
                actionButton(paste0("playerhit", i), "HIT"),
                actionButton(paste0("playerdouble", i), "DOUBLE"),
                actionButton(paste0("playerstand", i), "STAND"),
                col_widths = c(4, 4, 4)
              )
              ui_elements <- c(ui_elements, list(hand_controls))  # Ensure elements are added as list
            }
          }
          
          # print("UI elements prepared, checking final conditions")
          if (isTRUE(dealer$is21) || any(sapply(player$hands, function(hand) isTRUE(hand$is21)))) {
            # print("No controls shown: Dealer or a player has 21")
            return(NULL)
          } else {
            # print("Returning UI elements")
            # print(lapply(ui_elements, class))
            tagList(ui_elements)  # Directly pass the list to tagList
          }
        })
      }
      
      
      
      tableUI <- function() {
        bslib::card(
          bslib::card_header("Table"),
          bslib::card_body(
            uiOutput("playertotalcards")
          )
        )
      }
      
      ### not working
      playerCardUI <- function(player, input) {
        print("player card function working")
        
        if(is.null(player$hands) || length(player$hands) == 0) {
          print("No hands available")
          return(NULL)
        }
        
        cards_ui <- c()
        for (i in seq_along(player$hands)) {
          hands <- player$hands[[i]]
          if(is.null(hands$cards) || length(hands$cards) < 2) {
            print(sprintf("Insufficient cards in hand %d", i))
            next  # Skip this iteration
          }
          
          print(sprintf("Rendering cards for hand %d", i))
          bet_ui <- list(
            bslib::value_box(
              style = 'background-color: #DAA520!important; color: #0D6220;',
              title = sprintf("Bet for Hand %d", i),
              value = hands$bet,
              showcase = bsicons::bs_icon("coin"),
              style = "display: inline-block; margin-right: 10px;"
            )
          )
          cards_ui <- c(bet_ui)
          playing_cards_ui <- list(
            bslib::value_box(
              style = cardcolor(hands$cards[[1]]$Suit),
              title = hands$cards[[1]]$Suit,
              value = hands$cards[[1]]$Value,
              showcase = bsicons::bs_icon(paste0("suit-", hands$cards[[1]]$Icon, "-fill")),
              style = "display: inline-block; margin-right: 10px;"
            ),
            bslib::value_box(
              style = cardcolor(hands$cards[[2]]$Suit),
              title = hands$cards[[2]]$Suit,
              value = hands$cards[[2]]$Value,
              showcase = bsicons::bs_icon(paste0("suit-", hands$cards[[2]]$Icon, "-fill")),
              style = "display: inline-block; margin-right: 10px;"
            )
          )
          cards_ui <- c(bet_ui, playing_cards_ui)
          total_ui <- list(
            bslib::value_box(
              style = 'background-color: #DAA520!important; color: #0D6220;',
              title = "Total",
              value = hands$total,
              style = "display: inline-block; margin-right: 10px;"
            )
          )
          cards_ui <- c(bet_ui, playing_cards_ui, total_ui)
          if (length(hands$hitcards) > 0) {
            hit_cards_ui <- lapply(hands$hitcards, function(card) {
              bslib::value_box(
                style = cardcolor(card$Suit),
                title = card$Suit,
                value = card$Value,
                showcase = bsicons::bs_icon(paste0("suit-", card$Icon, "-fill")),
                style = "display: inline-block; margin-right: 10px;"
              )
            })
            cards_ui <- c(bet_ui, playing_cards_ui, hit_cards_ui, total_ui)
          } else {
            cards_ui <- c(bet_ui, playing_cards_ui, total_ui)
          }
        }
        
        div(style = "display: flex;", cards_ui)
      }
      
      
    
      