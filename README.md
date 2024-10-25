#create a owf database!#

#load data
info_p1 <- info[1:2,]
info_p2 <- info[3:18,]
info_p3 <- info[19:28,]



#load polygon shp
shapefile.1 <- sf::st_read("D:/r_stuffs/owf_old.shp")
shapefile.2 <- sf::st_read("D:/r_stuffs/owf_new.shp")
leaflet(st_transform(st_as_sf(shapefile.1),4326)) %>% addTiles() %>% addPolygons()
leaflet(st_transform(st_as_sf(shapefile.2),4326)) %>% addTiles() %>% addPolygons()

shapefile.1 <- sf::read_sf("D:/r_stuffs/owf_old.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')
shapefile.2 <- sf::read_sf("D:/r_stuffs/owf_new.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

shapefile.1 <- sf::st_as_sf(shapefile.1)  %>%
  mutate(fill_color = "lightblue")
shapefile.2 <- sf::st_as_sf(shapefile.2)  %>%
  mutate(fill_color = "lightgreen")



#set theme
theme_blue <- create_theme(
  bs4dash_color(
    olive = "#4A9094",
    blue = "#B0E2FF"
  ),
  bs4dash_status(
    primary = "#E1EDED",
    info = "#E4E4E4"
  )
)


# Define UI 
owfapp <- shinyApp(
  
ui <- bs4DashPage(
  
  title = "離岸風電辦公室",
  use_theme(theme_blue),
  
# Header  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "離岸風電資料庫",
      color = "olive",
      image = "https://www.gravel2gavel.com/files/2017/07/iStock-178449032-offshore-wind.jpg"
  )),


# Sidebar  
sidebar = dashboardSidebar(
  sidebarMenu(
    id = "sidebarMenuid",
    menuItem(
      "主頁",
      tabName = "主頁",
      icon = icon("home")
    ),
    menuItem(
      "目標及實績",
      tabName = "目標及實績",
      icon = icon("trophy")
    ),
    menuItem(
      "每日盤點",
      tabName = "每日盤點",
      icon = icon("clock")
    ),
    menuItem(
      "風場資訊",
      tabName = "風場資訊",
      icon = icon("wind"),
      menuSubItem("第一階段", tabName = "第一階段", icon = icon("location-dot")),
      menuSubItem("第二階段", tabName = "第二階段", icon = icon("location-dot")),
      menuSubItem("第三階段", tabName = "第三階段", icon = icon("location-dot"))
    )
  )
),

# Control bar ----
controlbar = dashboardControlbar(),

# Footer   
  footer = dashboardFooter(
    left = "經濟部離岸風電單一服務窗口",
    right = "版權所有 © 2024 工業技術研究院"
  ),

# Body  
  body = dashboardBody(
    tabItems(
      
      # Home tab ----
  tabItem(
        tabName = "主頁",
        
        jumbotron(
          title = "Welcome!",
          status = "info",
          lead = "歡迎來到工研院離岸風電單一服務窗口",
          href = "https://www.twtpo.org.tw/"
          ),
        
        #add map
        fluidPage(
          leafletOutput(outputId = "map")
          ),
        
  fluidRow(
        userBox(
          collapsible = FALSE,
          title = userDescription(
            title = "工研院綠能暨環境研究所",
            image = "https://tse3.mm.bing.net/th/id/OIP.9l08nrjahlTm56OyVDGJXAHaHa?w=922&h=923&rs=1&pid=ImgDetMain",
            type = 2
          ),
          status = "primary"
          ),
        
        box(
          title = "經濟部能源署",
          width = 6,
          height = 4,
          collapsible = FALSE
           ),

      ),
  ),
  
      # Dashboard tab1 ----
  tabItem(
        tabName = "目標及實績",
        
        ## Info boxes ----
        fluidRow(
          
          column(
            width = 4,
            infoBox(
              width = 12,
              value = "2,705.2-3,653.2 MW",
              title = "2024目標量",
              color = "orange",
              icon = icon("bullseye"),
            )
          ),
          
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "已安裝風機容量",
              value = "2978.9 MW",
              color = "lightblue",
              icon = icon("bolt"),
            )
          ),
          
          column(
            width = 4,
            infoBox(
              width = 12,
              value = "366 座",
              title = "已安裝風機量",
              color = "lightblue",
              icon = icon("wind"),
            )
          )
        ),
  ), 
        
   # Dashboard tab2 ---- 
    tabItem(
          tabName = "每日盤點",
          
          ## Info boxes ----
          fluidRow(
            
            column(
              width = 6,
              infoBox(
                width = 12,
                title = "每日更新",
                value = dateInput("日期", "日期:"),
                color = "lightblue",
                icon = icon("clock"),
              )
            )
            ),
          
          fluidRow( 
            column(
              width = 6,
              infoBox(
                width = 12,
                title = "今日新增風機容量",
                value = " MW",
                color = "lightblue",
                icon = icon("bolt"),
              )
            ),
            
            column(
              width = 6,
              infoBox(
                width = 12,
                title = "今日新增風機量",
                value = " 座",
                color = "lightblue",
                icon = icon("wind"),
              )
            )
          ),
    ),
    # Dashboard tab3 ---- 
    tabItem(tabName = "第一階段",
            fluidRow(
              DTOutput("owf_info_p1")
            )),
    tabItem(tabName = "第二階段",
            fluidRow(
              DTOutput("owf_info_p2")
              )),
    tabItem(tabName = "第三階段",
            fluidRow(
              DTOutput("owf_info_p3")
            ))

  )
)
),

# Define server
server <- function(input, output){
  
  output$map <- renderLeaflet({
    leaflet(shapefile.1) %>% 
      
      setView(lng = 120, lat = 23.5, zoom = 7) %>%
      
      addPolygons(data=shapefile.1, 
                  fillColor = "blue",
                  fillOpacity = 0.5, 
                  color = "blue") %>% 
      
      addPolygons(data=shapefile.2, 
                  fillColor = "lightgreen",
                  fillOpacity = 0.5, 
                  color = "green") %>% 
     
       addProviderTiles('Esri.WorldImagery') %>% 
     
       addTiles() 
    
  })
  
  output$owf_info_p1 <- renderDT(owf_info_p1)
  
  output$owf_info_p2 <- renderDT(owf_info_p2)
  
  output$owf_info_p3 <- renderDT(owf_info_p3)
}


)


runApp(owfapp)


<!---
heyyyjoyyy/heyyyjoyyy is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
