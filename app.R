
# split cards (Turn table into card in card ui output)
# double down
# add player
    # might be worth looing into rhino from here on out.
# post to shiny io and move on to bacarrat

library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(DT)
source("functions.r")

ui <- bslib::page_fillable(
  theme = apptheme(),
  useShinyjs(),
  dealerUI(),
  tableUI(),
  playerUI()
)

server <- function(input, output, session) {
  appdata <- initData("appdata")
  dealer <- initData("dealer")
  player <- initData("player")
  
  observeEvent(input$buyinplayer, {
    buyinGame(player, input)
  })
  
  onclick('bankroll', { 
    print("[1] start bankroll update")
    buyinGame(player, input)
    print("[2] finish bankroll update")
  })
  
  observeEvent(player$newcards == TRUE, {
    print("player card observe triggered")
    output$playertotalcards <- renderUI({
      print("render ui working")
      playerCardUI(player, input)
    })
  })
  
  observeEvent(dealer$newcards == TRUE, {
    output$dealertotalcards <- renderUI({
      dealerCardUI(dealer)
    })
  })
  
  output$playercurrentbuy <- renderUI({
    buyinUI(player)
  })
  
  observeEvent(input$start, {
    print("[3] pre require")
    req(player$buyin > 0)  # Ensure there is a buy-in before proceeding
    print("[4] post require")
    print("pre set bet")
    setBet(player, hand_index = 1, input)
    # print("post set bet")
    # print("[5] pre deal")
    dealGame(appdata, player, dealer)
    # print("[6] post deal")
    # print("[7] pre match check")
    checkMatch(player, output, hand_index = 1)
    # print("[8] post match check")
    # print("[9] pre royal check")
    checkRoyalMatch(player, hand_index = 1)
    # print("[10] post royal check")
    # print("[11] pre check21")
    check21(dealer, player)
    # print("[12] post check21")
    # print("[13] pre control output")
    outputControls(dealer, player, output)
    # print("[14] post control output")
  })
  
  player_hands <- reactive({
    return(player$hands)
  })
  
  observe({
    # Now player_hands() is used, which is a proper reactive call
    for (i in seq_along(player_hands())) {
      local({
        local_i <- i
        observeEvent(input[[paste0("playerhit", local_i)]], {
          playerHit(appdata, player, local_i)
          checkRematch(player, local_i, input)
          checkBust(player, local_i, appdata, output)
        })
        
        observeEvent(input[[paste0("playerdouble", local_i)]], {
          playerDouble(appdata, player, local_i, dealer, output)
        })
        
        observeEvent(input[[paste0("playerstand", local_i)]], {
          finishDealerHand(dealer, appdata)
          determineGameResult(player, dealer)
          output$actioncontrols <- renderUI(NULL)
        })
      })
    }
  })
  
  
  observeEvent(input$restart, {
    clearGame(appdata, dealer, player, output)
  })
  
  observeEvent(appdata$reshuffle, {
    reshuffleCards(appdata)
  })
}


shinyApp(ui, server)


