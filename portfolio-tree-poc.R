library(xtable)
library(shiny)
library(shinyBS)

options(DT.options = list(dom = 'tpl', rownames = FALSE, pageLength = 10),
        xtable.include.rownames = F)


{
  table_script <- "$('.tree').treegrid();
  $('.tree').treegrid('collapseAll');
  $('.treegrid-1').treegrid('expand');
  
  $('.tree tr').each(
  function(){
  $(this).find('td').slice(8,13).hide();
  $(this).find('th').slice(8,13).hide();
  });
  
  $('.tree tr').first().find('th:eq(2)').hide();
  
  $(' .tree tbody td:nth-of-type(4)').each(function() {
  if (Number($(this).text()) < 0) {
  $(this).addClass('negRtn');
  } else if(Number($(this).text()) > 0){
  $(this).addClass('posRtn');
  } else {
  $(this).addClass('neuRtn');
  }
  });
  
  $(' .tree tbody td:nth-of-type(7)').each(function() {
  if (Number($(this).text()) < 0) {
  $(this).addClass('negRtn');
  } else if(Number($(this).text()) > 0){
  $(this).addClass('posRtn');
  } else {
  $(this).addClass('neuRtn');
  }
  });
  
  var portfolio = $(this).closest('tr').children('td:first').text();
  var horizon = $(this).parent().children().index($(this));
  
  "
}

external_javascript <- "<link href='https://gitcdn.github.io/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css' rel='stylesheet'>
    <script src='https://gitcdn.github.io/bootstrap-toggle/2.2.2/js/bootstrap-toggle.min.js'></script>
    <link rel='stylesheet' href='https://cdn.rawgit.com/maxazan/jquery-treegrid/master/css/jquery.treegrid.css'>
    <script type='text/javascript' src='https://cdn.rawgit.com/maxazan/jquery-treegrid/master/js/jquery.treegrid.js'></script>
    <script type='text/javascript' src='https://cdn.rawgit.com/maxazan/jquery-treegrid/master/js/jquery.treegrid.bootstrap3.js'></script>
    "

convert_to_tree <- function(df) {
  y <-
    paste(capture.output(print(xtable(df), type = "html"))[-c(1, 2)],
          sep = "",
          collapse = "")
  x <- as.numeric(gregexpr("</tr>  <tr>", y)[[1]])
  yy <- 0
  yyy <- 0
  for (i in 1:length(x)) {
    if (df$long_name[i] %in% c("Total Fund")) {
      y <-
        sub(
          "</tr>  <tr>",
          paste0(
            "</tr><tr class='treegrid-",
            i,
            " aggs treegrid-expanded'>"
          ),
          y
        )
      yy <- yy + 1
      
    } else if (df$long_name[i] %in% c("Equity",
                                      "Fixed Income")) {
      y <-
        sub(
          "</tr>  <tr>",
          paste0(
            "</tr><tr class='treegrid-",
            i,
            " treegrid-parent-1 aggs2'>"
          ),
          y
        )
      yy <- i
      yyy <- i
    } else if (df$long_name[i] %in% c("Domestic",
                                      "Foreign",
                                      "Active",
                                      "Passive")) {
      y <-
        sub(
          "</tr>  <tr>",
          paste0(
            "</tr><tr class='treegrid-",
            i,
            " treegrid-parent-",
            yyy,
            " aggs3'>"
          ),
          y
        )
      yy <- i
      yyyy <- i
    } else if (df$long_name[i] %in% c('Level')) {
      
      y <-
        sub(
          "</tr>  <tr>",
          paste0(
            "</tr><tr class='treegrid-",
            i,
            " treegrid-parent-",
            yyyy,
            " aggs4'>"
          ),
          y
        )
      yy <- i
    } else {
      y <-
        sub(
          "</tr>  <tr>",
          paste0(
            "</tr><tr class='treegrid-",
            i,
            " treegrid-parent-",
            yy,
            "'>"
          ),
          y
        )
    }
  }
  
  y <- sub(
    "<table border=1>",
    "<table width='100%' class='tree'><tr>
             <td colspan='1' width='25%' style='border: 0px'></td>
             <th colspan='3' width='32.5%' align='center'>DAY</th>
             <th colspan='3'  width='32.5%' align='center'>MTD</th>
             <td colspan='1'  width='10%' style='border: 0px'></td>
             </tr>",
    y
  )
  
  return(y)
  
}

#JavaScript and misc HTML, css
{
  javaScript <- "<script>
  
  Shiny.addCustomMessageHandler('jsCode', function(message) { 
  $('table.tree').replaceWith(message.perf_table); 
  eval(message.value); 
  });
  
  
  </script>
  <style>
  .modal-lg {width: 97.5%;}
  .tree table { 
  width: 90%; 
  border-collapse: collapse; 
  margin:50px auto;
  }
  .tree th { 
  background: #487eb0; 
  color: white; 
  font-weight: bold; 
  }
  .tree td,.tree  th { 
  padding: 6px; 
  border: 1px solid black; 
  text-align: center; 
  font-size: 15px;
  }
  .tree  td:first-child,
  .tree th:first-child{
  text-align: left; 
  font-weight: bold;
  border: 1px solid black;
  }
  .aggs td, .aggs2 td, 
  .aggs3 td {
  font-weight:bold;
  border-bottom-style: double;
  }
  .aggs td:nth-of-type(1){
  background-color: #9b59b6;
  font-weight: bold;
  color: #fff;
  }
  .aggs2 td:nth-of-type(1){
  background-color: #3498db;
  font-weight: bold;
  color: #fff;
  }
  .aggs3 td:nth-of-type(1){
  background-color: #2ecc71;
  font-weight: bold;
  color: #fff;
  }
  .aggs4 td:nth-of-type(1){
  background-color: #1abc9c;
  font-weight: bold;
  color: #fff;
  }
  .tree tbody td:nth-of-type(4),
  .tree thead th:nth-of-type(4),
  .tree tbody td:nth-of-type(7),
  .tree thead th:nth-of-type(7){
  background: #fdf9a5;
  }
  .posRtn{
  color:#4bbe0e; font-weight: bold;
  }
  .negRtn{
  color:red; font-weight: bold;
  }
  .neuRtn{
  color:black; font-weight: bold;
  }
  .compWarn{
  color:black; font-weight: bold; background-color:yellow;
  }
  .compAlert{
  color:black; font-weight: bold; background-color:red;
  }
  .tree tbody td:nth-of-type(4):hover {color:#b078c7;}
  .tree tbody td:nth-of-type(7):hover {color:#b078c7;}
  
  .factors {width:90%; text-align:center;}
  .factors th{width:10%; text-align:center;}
  .factors th:nth-child(1){width:15%;}
  .factors td{text-align:center;}
  .factors td:hover {font-weight: bold;}
  
  .top {width:90%; text-align:center; }
  .top th{width:10%; text-align:center; padding:5px;}
  .top th:nth-child(1){width:35%;}
  .top td{text-align:center;}
  .top tr:nth-child(-n+6){background:#B7ECB8;}
  .top tr:nth-child(n+7){background:#ecb7b7;}
  .top tr:nth-child(1){background:white;}
  </style>
  <meta http-equiv='Pragma' content='no-cache'>
  <meta http-equiv='Expires' content='-1'>
  <meta http-equiv='CACHE-CONTROL' content='NO-CACHE'>
  "
}

# ui ----
ui <- fluidPage(tags$head(HTML(paste0(
  external_javascript, javaScript
))),
titlePanel("Why are trees"),

fluidRow(
  HTML("<table class='tree'></table>")
)
)


server <- function(input,output, session){
  
  
  perf_data <- reactive({
    
    x <- data.frame(
      long_name = c('Total Fund',
                    'Equity',
                    'Domestic',
                    paste0('DE port ', 1:3),
                    'Foreign',
                    paste0('FE port ', 1:3),
                    'Fixed Income',
                    'Active',
                    paste0('FI active port ', 1:3),
                    'Passive',
                    paste0('FI passive port ', 1:3)
      ),
      dtd_rtn = runif(19, min = -0.025, max = 0.035),
      dtd_bmk = runif(19, min = -0.025, max = 0.035),
      mtd_rtn = runif(19, min = -0.045, max = 0.065),
      mtd_bmk = runif(19, min = -0.045, max = 0.065),
      nav = runif(19, min = 1000, max = 25500)
    )
    
    x$dtd_active <- x$dtd_rtn - x$dtd_bmk
    x$mtd_active <- x$mtd_rtn - x$mtd_bmk
    
    x <- x[,c(1:3,7,4,5,8,6)]
    return(x)
    
  })
  
  
  
  #Refresh tree table on date change
  observe({
    
    morning_perf <- perf_data()
    morning_perf <- convert_to_tree(morning_perf)
    morning_perf <- htmltools::HTML(morning_perf)
    
    session$sendCustomMessage(type='jsCode', list(value = table_script, perf_table = morning_perf))
  })
  
}

shinyApp(ui = ui, server = server)