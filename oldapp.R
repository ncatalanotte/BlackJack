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
source("oldfunctions.r")

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
  
  onclick('bankroll', { 
    buyinGame(player, input)
  })
  
  output$playercurrentbuy <- renderUI({
    buyinUI(player)
  })
  
  observeEvent(player$newcards == TRUE, {
    output$playertotalcards <- renderUI({
      playerCardUI(player, input)
    })
  })
  
  observeEvent(dealer$newcards == TRUE, {
    output$dealertotalcards <- renderUI({
      dealerCardUI(dealer)
    })
  })
  
  observeEvent(input$start, {
    if(player$buyin == 0) {
      noBuyinModal()
    } else {
      setBet(player, input, dealer)
      dealGame(appdata, player, dealer)
      checkMatch(player, output)
      checkRoyalMatch(player)
      check21(dealer, player)
      outputControls(dealer, player, output)
    }
  })
  
  observeEvent(input$playerhit, {
    playerHit(appdata, player)
    checkRematch(player, input)
    checkBust(player, appdata)
  })
  
  observeEvent(input$playerstand, {
    output$actioncontrols <- renderUI(NULL)
    dealer$showsecondcard <- TRUE
    finishDealerHand(dealer, appdata)
    dealer$gamefinished <- TRUE
    determineGameResult(player, dealer)
    updateDeckCount(appdata)
  })
  
  observeEvent(input$restart, {
    clearGame(appdata, dealer, player, output)
  }) 
  
  observeEvent(appdata$reshuffle == TRUE, {
    reshuffleCards(appdata)
  })
}

shinyApp(ui, server)