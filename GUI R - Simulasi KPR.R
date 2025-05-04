#install library yang akan digunakan 
install.packages('shiny')
install.packages('ggplot2')

#buka library
library(shiny)
library(ggplot2)

#fungsi untuk menghitung angsuran bulanan (anuitas biasa)
anuitas_biasa <- function(harga_rumah, suku_bunga, down_payment, tahun) {
  pv <- harga_rumah - ((down_payment/100)*harga_rumah)
  suku_bunga_bulanan <- suku_bunga / 12 / 100
  n <- tahun * 12
  return(pv / ((1 - (1 + suku_bunga_bulanan)^(-n))/suku_bunga_bulanan))
}

#fungsi untuk menghitung angsuran bulanan (anuitas dimuka)
anuitas_dimuka <- function(harga_rumah, suku_bunga, down_payment, tahun) {
  pv <- harga_rumah - ((down_payment/100)*harga_rumah)
  suku_bunga_bulanan <- suku_bunga / 12 / 100
  n <- tahun * 12
  return(pv / (((1 - (1 + suku_bunga_bulanan)^(-n+1))/suku_bunga_bulanan)+1))
}

#fungsi untuk menghitung angsuran bulanan (anuitas ditunda)
anuitas_ditunda <- function(harga_rumah, suku_bunga, down_payment, tunda, tahun) {
  pv <- harga_rumah - ((down_payment/100)*harga_rumah)
  suku_bunga_bulanan <- suku_bunga / 12 / 100
  n <- tahun * 12
  return(pv / (((1 - (1 + suku_bunga_bulanan)^(-n))/(suku_bunga_bulanan*(1 + suku_bunga_bulanan)^(tunda-1)))))
}

#fungsi untuk membuat data per bulan dengan data frame
data_perbulan <- function(harga_rumah, suku_bunga, down_payment, tahun, anuitas, jenis_anuitas, tunda = 0) {
  suku_bunga_bulanan <- suku_bunga / 12 / 100
  n <- tahun * 12
  balance <- if (jenis_anuitas == "ditunda") harga_rumah * (1 + suku_bunga_bulanan)^tunda else harga_rumah
  
  #membuat data frame kosong
  details <- data.frame(
    Bulan = numeric(),
    Pembayaran = numeric(),
    Suku_bunga = numeric(),
    Harga_pokok = numeric(),
    Balance = numeric(),
    stringsAsFactors = FALSE
  )
  
  #mengisi data frame
  for (i in 1:n) {
    pembayaran_bunga <- balance * suku_bunga_bulanan
    pembayaran_pokok <- anuitas - pembayaran_bunga
    balance <- balance - pembayaran_pokok
    
    details <- rbind(details, data.frame(
      Bulan = i,
      Pembayaran = round(anuitas, 2),
      Suku_bunga = round(pembayaran_bunga, 2),
      Harga_pokok = round(pembayaran_pokok, 2),
      Balance = round(balance, 2)
    ))
  }
  
  return(details)
}

#Membuat UI
ui <- fluidPage(
  #membuat tempat judul dan deskripsi
  tags$head(
    tags$style(HTML("
      h1 { text-align: center; margin-top: 20px; margin-bottom: 40px; }
      .description { margin-top: 10px; }
    "))
  ),
  
  #judul projek
  tags$h1("Simulasi KPR - Anuitas Biasa, Dimuka dan Ditunda"),
  
  #membuat inputan
  sidebarLayout(
    sidebarPanel(
      numericInput("rumah", "Harga Rumah (Rp)", value = 500000000),
      numericInput("DP", "Uang Muka (%)", value = 20),
      numericInput("suku_bunga", "Suku Bunga (%)", value = 5),
      numericInput("tahun", "Jangka Waktu (Tahun)", value = 15),
      radioButtons("jenis_anuitas", "Pilih Jenis Anuitas:",
                   choices = c("Anuitas Biasa" = "biasa", "Anuitas Dimuka" = "dimuka", "Anuitas Ditunda" = "ditunda")),
      numericInput("tunda", "Periode Penundaan (Bulan)", value = NULL, min = 0, max = 24),
      numericInput("bulan", "Pilih Bulan", value = 1),
      actionButton("hitung", "Hitung")
    ),
    
    #membuat deskripsi dan output
    mainPanel(
      tags$div(class = "description", h3("Penjelasan Jenis Anuitas"),
               tags$p(tags$b("Anuitas Biasa:"), "Waktu pembayaran atau penerimaan dilakukan di akhir setiap periode."),
               tags$p(tags$b("Anuitas Dimuka:"), "Waktu pembayaran atau penerimaan dilakukan di awal setiap periode."),
               tags$p(tags$b("Anuitas Ditunda:"), "Waktu pembayaran atau penerimaan dilakukan setelah beberapa periode (harap mengisi periode penundaan).")
      ),
      verbatimTextOutput("anuitas"), 
      verbatimTextOutput("bulan_anuitas"), #agar bisa 1 kalimat perbaris
      plotOutput("pie_chart")
    )
  )
)

#membuat servernya
server <- function(input, output) {
  #mendeskripsikan input
  observeEvent(input$hitung, {
    harga_rumah <- input$rumah
    down_payment <- input$DP
    suku_bunga <- input$suku_bunga
    tahun <- input$tahun
    jenis_anuitas <- input$jenis_anuitas
    tunda <- input$tunda
    bulan <- input$bulan
    
    # Reset output sehingga bila salah mengisi harga rumah, suku bunga, tahun dll hasilnya akan kosong
    output$anuitas <- renderText({ "" })
    output$bulan_anuitas <- renderText({ "" })
    output$pie_chart <- renderPlot({ NULL })
    
    #validasi input sesuai syarat 
    validate(
      need(harga_rumah > 0, "Jumlah kredit harus lebih dari 0"),
      need(suku_bunga > 0, "Suku bunga harus lebih dari 0"),
      need(tahun > 0, "Jangka waktu harus lebih dari 0"),
      need(bulan >= 1 && bulan <= tahun * 12, "Bulan harus berada dalam jangka waktu pinjaman"),
      need(!(jenis_anuitas == "ditunda" && is.null(tunda)), 
           "Harus mengisi periode penundaan untuk Anuitas Ditunda")
    )
    
    
    #perhitungan angsuran bulanan berdasarkan jenis anuitas
    if (jenis_anuitas == "biasa") {
      anuitas <- anuitas_biasa(harga_rumah, suku_bunga, down_payment, tahun)
      anuitas_text <- "Anuitas Biasa"
    } else if (jenis_anuitas == "dimuka") {
      anuitas <- anuitas_dimuka(harga_rumah, suku_bunga, down_payment, tahun)
      anuitas_text <- "Anuitas Dimuka"
    } else {
      anuitas <- anuitas_ditunda(harga_rumah, suku_bunga, down_payment, tunda, tahun)
      anuitas_text <- paste("Anuitas Ditunda (",tunda," bulan)")
    }
    
    #rincian perbulan
    detail_bulan <- data_perbulan(harga_rumah, suku_bunga, down_payment, tahun, anuitas, jenis_anuitas, tunda)
    data_bulan_terpilih <- detail_bulan[detail_bulan$Bulan == bulan, ]
    
    #output hasil perhitungan
    output$anuitas <- renderText({
      paste("Angsuran Bulanan (",anuitas_text,"): Rp", round(anuitas, 2))
    })
    
    output$bulan_anuitas <- renderText({
      paste("Bulan :", data_bulan_terpilih$Bulan,
            "\nPembayaran : Rp", round(data_bulan_terpilih$Pembayaran, 2),
            "\nPokok : Rp", round(data_bulan_terpilih$Harga_pokok, 2),
            "\nBunga : Rp", round(data_bulan_terpilih$Suku_bunga, 2))
    })
    
    #pie chart perbandingan
    output$pie_chart <- renderPlot({
      pie_data <- data.frame(
        Category = c("Pokok", "Bunga"),
        Amount = c(data_bulan_terpilih$Harga_pokok, data_bulan_terpilih$Suku_bunga)
      )
      
      ggplot(pie_data, aes(x = "", y = Amount, fill = Category)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void() +
        scale_fill_manual(values = c("salmon", "cyan")) +
        labs(title = paste("Perbandingan Pembayaran di Bulan", bulan)) +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    })
  })
}

#menjalankan aplikasi
shinyApp(ui = ui, server = server)