# Code SERVER or Back End Display in RShiny 

server = function(input, output){

  #Function to create covariate matrix (Matrix of X)
  matrix_X = function(dataframe_new, col.selected){
    matrix.data = matrix(NA, nrow = nrow(dataframe_new), ncol = length(col.selected)*nrow(dataframe_new))
    for (i in 1:nrow(dataframe_new)){
      for (j in 1:(length(col.selected)*nrow(dataframe_new))){
        if(j>length(col.selected)*(i-1) & j<=length(col.selected)*i){
          matrix.data[i,j] = dataframe_new[i, col.selected[(j-length(col.selected)*(i-1))]]
        }else{
          matrix.data[i,j] = as.numeric(0)
        }
      }
    }
    return(matrix.data)
  }

  #Function to create Identity matrix as much as the size of covariate matrix (Matrix of X)
  matrixJ = function(dataframe_new,col.selected){
    x = diag(length(col.selected)*2)
    for (i in 1:(nrow(dataframe_new)-1)){
      x = rbind(x, diag(length(col.selected)*2))
    }
    return(x)
  }

  hideTab('tab', 'Vector & Matrix', session = getDefaultReactiveDomain())
  hideTab('tab', 'Result of Prediction', session = getDefaultReactiveDomain())
  hideTab('tab', 'Download Data', session = getDefaultReactiveDomain())

  checkTab = reactive({
    if(length(input$variableZ) == 0){
      hideTab('tabm', 'Matrix of Z', session = getDefaultReactiveDomain())
    }
  })


  dataframe = reactive({
    file.data = input$upload
    if(is.null(file.data)){
      return(NULL)
    }else{
      dataFrame = read.csv(file.data$datapath, sep = input$separator, header = input$header)
    }
  })

  output$printData = renderDT({
    dataFrame = dataframe()
    if(length(dataFrame)!=0){
      showTab('tab', 'Vector & Matrix', session = getDefaultReactiveDomain())
      showTab('tab', 'Result of Prediction', session = getDefaultReactiveDomain())
      showTab('tab', 'Download Data', session = getDefaultReactiveDomain())
    }
    datatable(dataFrame, options = list(
      scrollX = TRUE))
  })

  output$columeNames = renderText({
    dataFrame = dataframe()
    colnames(dataFrame)
  })

  #Function to display entries from Vector of Y
  output$optionColumnY = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      return(NULL)
    }else{
      div(id = "colY", style="margin-top:30px;",
          checkboxGroupInput("variableY", "Select Column Name for Vector of Y:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })
  output$inputrowY= renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowY', "Number of rows to display: "), span('from '), span(as.character(nrow(dataFrame))), span("rows"))
  })

  Y = reactive({
    dataFrame = dataframe()
    matrix = dataFrame[,input$variableY]
    return(matrix)
  })

  output$vectorY = renderDataTable({
    if(length(input$variableY)==1){
      matrix = Y()
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowY)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowY,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)

  #Function to display entries from Matrix of X
  output$OptionColumnX = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      h4("")
    }else{
      div(id = "colX", style="margin-top:30px;",
        checkboxGroupInput("variableX", "Select Column Name for Matrix of X:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })
  output$inputrowX = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowX', "Number of rows to display: "), span('from '), span(as.character(nrow(dataFrame))), span("rows"))
  })
  output$inputcolX = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numcolX', "Number of columns to display: ",), span('from '), span(as.character(length(input$variableX)*nrow(dataFrame))), span("columns"))
  })


  X = reactive({
    dataFrame = dataframe()
    matrix = matrix_X(dataframe_new =dataFrame, col.selected = input$variableX)
    #return(matrix)
  })
  output$matrixX = renderDT({
    if(length(input$variableX)!=0){
      matrix = X()
      matrix = data.frame(matrix)
      if(!isTruthy(input$numrowX) & !isTruthy(input$numcolX)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowX)){
        matrix = matrix[1:6,1:input$numcolX]
      }else if(!isTruthy(input$numcolX)){
        matrix = matrix[1:input$numrowX,1:6]
      }else{
        matrix = matrix[1:input$numrowX,1:input$numcolX]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })

  #Function to display entries from Matrix of Z
  output$optionColumnZ = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      return(NULL)
    }else{
      div(id = "colZ", style="margin-top:30px;",
          checkboxGroupInput("variableZ", "Select Column Name for Matrix of Z:", inline=TRUE, colnames(dataFrame)),
          hr(),
      )
    }
  })
  output$inputrowZ = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowZ', "Number of rows to display: "), span('from '), span(as.character(nrow(dataFrame))), span("rows"))
  })
  output$inputcolZ = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numcolZ', "Number of columns to display: ",), span('from '), span(as.character(length(input$variableZ)*nrow(dataFrame))), span("columns"))
  })

  Z = reactive({
    dataFrame = dataframe()
    matrix = matrix_X(dataframe_new =dataFrame, col.selected = input$variableZ)
    #return(matrix)
  })

  output$matrixZ = renderDT({
    if(length(input$variableZ)!=0){
      matrix = Z()
      matrix = data.frame(matrix)
      if(!isTruthy(input$numrowZ) & !isTruthy(input$numcolZ)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowZ)){
        matrix =  matrix[1:6,1:input$numcolZ]
      }else if(!isTruthy(input$numcolZ)){
        matrix = matrix[1:input$numrowZ,1:6]
      }else{
        matrix = matrix[1:input$numrowZ,1:input$numcolZ]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })

  #Function to display entries from Matrix of J
  output$optionColumnJ = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      return(NULL)
    }else{
      div(id = "colJ", style="margin-top:30px;",
          checkboxGroupInput("variableJ", "Select Column Name for Matrix of J:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })
  output$inputrowJ = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowJ', "Number of rows to display: "), span('from '), span(as.character(length(input$variableJ)*2*nrow(dataFrame))), span("rows"))
  })
  output$inputcolJ = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numcolJ', "Number of columns to display: ",), span('from '), span(as.character(length(input$variableJ)*2)), span("columns"))
  })

  J = reactive({
    dataFrame = dataframe()
    matrix = matrixJ(dataframe_new = dataFrame, col.selected = input$variableJ)
    return(matrix)
  })
  output$matrixJ = renderDT({
    if(length(input$variableJ)!=0){
      matrix = J()
      matrix = data.frame(matrix)
      if(!isTruthy(input$numrowJ) & !isTruthy(input$numcolJ)){
        if(nrow(matrix)<=6 | ncol(matrix)<=6){
          matrix = matrix[1:nrow(matrix),1:ncol(matrix)]
        }else{
          matrix = matrix[1:6,1:6]
        }
      }else if(!isTruthy(input$numrowJ)){
        matrix = matrix[1:6,1:input$numcolJ]
      }else if(!isTruthy(input$numcolJ)){
        matrix = matrix[1:input$numrowJ,1:6]
      }else{
        matrix = matrix[1:input$numrowJ,1:input$numcolJ]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })

  #Function to display the result of multiplying Kronecker Matrikz of Z with Identity matrix
  ZI = reactive({
    dataFrame = dataframe()
    # Generate latitude and longitude diagonal matrix
    matrixZOld = matrix_X(dataframe_new = dataFrame, col.selected = input$variableZ)
    # Kronecker multiplication of Matrix of Z and the Identity matrix of size covariate matrix of X
    matrixZNew = kronecker(matrixZOld, diag(length(input$variableX)))
    return(matrixZNew)
  })

  output$warningZI = renderText({
    if(length(input$variableZ) == 0){
      return("Please fill in the Matrix of Z")
    }else{
      return(NULL)
    }
  })
  output$inputrowZI = renderUI({
    if(length(input$variableZ) == 0){
      return(NULL)
    }else{
      matrix = data.frame(ZI())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowZI', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  output$inputcolZI = renderUI({

    if(length(input$variableZ) == 0){
      return(NULL)
    }else{
      matrix = data.frame(ZI())
      div(style="display: inline-block;vertical-align:top;", textInput('numcolZI', "Number of columns to display: ",), span('from '), span(as.character(ncol(matrix))), span("columns"))
    }
  })

  output$matrixZI = renderDT({
    if(length(input$variableZ)!=0){
      matrix = data.frame(ZI())
      if(!isTruthy(input$numrowZI) & !isTruthy(input$numcolZI)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowZI)){
        matrix = matrix[1:6,1:input$numcolZI]
      }else if(!isTruthy(input$numcolZI)){
        matrix = matrix[1:input$numrowZI,1:6]
      }else{
        matrix = matrix[1:input$numrowZI,1:input$numcolZI]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })

  #Function to calculate Weight Matrix of W (Inverse Distance Matrix)
  normXY = function(X,Y){
    norm = matrix(NA, nrow = length(X), ncol = length(Y))
    for (i in 1:length(X)){
      for (j in 1:length(Y)){
        norm[i,j] = sqrt((X[i]-X[j])^2+(Y[i]-Y[j])^2)
      }
    }
    return(norm)
  }

  weighted = function(X,Y){
    weight = matrix(NA, nrow = length(X), ncol = length(Y))
    norm = normXY(X,Y)
    for (i in 1:length(X)){
      for (j in 1:length(Y)){
        if(i==j){
          weight[i,j] = as.numeric(0)
        }else{
          weight[i,j] = 1/(norm[i,j])^2
        }
      }
    }
    return(weight)
  }

  inverseWeighted = function(X,Y){
    inv.weighted =  matrix(NA, nrow = length(X), ncol = length(Y))
    weight.matrix = weighted(X,Y)
    total.row = rowSums(weight.matrix)
    for (i in 1:length(X)){
      for (j in 1:length(Y)){
        inv.weighted[i,j] = weight.matrix[i,j]/total.row[i]
      }
    }
    return(inv.weighted)
  }
  W = reactive({
    dataFrame = dataframe()
    if(length(input$variableW)==2){
      matrixW = inverseWeighted(dataFrame[,input$variableW[1]], dataFrame[,input$variableW[2]])
    }
    return(matrixW)
  })
  output$optionColumnW = renderUI({
    dataFrame = dataframe()
    if(length(dataFrame)==0){
      return(NULL)
    }else{
      div(id = "colW", style="margin-top:30px;",
          checkboxGroupInput("variableW", "Select Column Name for Matrix of W:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })


  output$inputrowW = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numrowW', "Number of rows to display: "), span('from '), span(as.character(nrow(dataFrame))), span("rows"))
  })
  output$inputcolW = renderUI({
    dataFrame = dataframe()
    div(style="display: inline-block;vertical-align:top;", textInput('numcolW', "Number of columns to display: ",), span('from '), span(as.character(nrow(dataFrame))), span("columns"))
  })


  output$matrixW = renderDT({
    print(input$variableW)
    if(length(input$variableW)==2){
      matrix = data.frame(W())
      if(!isTruthy(input$numrowW) & !isTruthy(input$numcolW)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowW)){
        matrix = matrix[1:6,1:input$numcolW]
      }else if(!isTruthy(input$numcolW)){
        matrix = matrix[1:input$numrowW,1:6]
      }else{
        matrix = matrix[1:input$numrowW,1:input$numcolW]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })
  rhoBeta <<- list()
  rho = reactive({
    X = X()
    W = W()
    Y = Y()
    A = A()
    Y = Y()
    ZI = ZI()
    J = J()
    rho = solve(t(Y)%*%t(W)%*%W%*%Y)%*%t(Y)%*%t(W)%*%Y
    rhoBeta <<-list("rho" = rho)
    b0 = inv(t(A)%*%A)%*%t(A)%*%Y-(inv(t(A)%*%A)%*%t(A)%*%W%*%Y)*as.numeric(rhoBeta["rho"])
    rhoBeta <<-list("rho" = rho, "b0" = b0)
    beta = ZI%*%J%*%as.numeric(unlist(rhoBeta$b0))
    rhoBeta <<-list("rho" = rho, "b0" = b0, "beta" = beta)
    rhoNew = solve(t(W%*%Y)%*%W%*%Y)%*%t(W%*%Y)%*%(Y-X%*%as.numeric(unlist(rhoBeta$beta)))
    rhoBeta <<-list("rho" = rho, "b0" = b0, "beta" = beta, "rhoNew" = rhoNew)
    tol = 0.001
    error = abs(rho-rhoNew)
    while(error>tol){
      rho = as.numeric(rhoBeta["rhoNew"])
      rhoBeta["rho"] = rho
      b0 = inv(t(A)%*%A)%*%t(A)%*%Y-(inv(t(A)%*%A)%*%t(A)%*%W%*%Y)*as.numeric(rhoBeta["rho"])
      rhoBeta <<-list("rho" = rho, "b0" = b0, "beta" = beta, "rhoNew" = rhoNew)
      beta = ZI%*%J%*%as.numeric(unlist(rhoBeta$b0))
      rhoBeta <<-list("rho" = rho, "b0" = b0, "beta" = beta, "rhoNew" = rhoNew)
      rhoNew = solve(t(W%*%Y)%*%W%*%Y)%*%t(W%*%Y)%*%(Y-X%*%as.numeric(unlist(rhoBeta$beta)))
      rhoBeta <<-list("rho" = rho, "b0" = b0, "beta" = beta, "rhoNew" = rhoNew)
      error = rho-as.numeric(unlist(rhoNew[1,1]))
    }
    return(as.numeric(unlist(rhoBeta["rho"])))
  })

  output$rho = renderText({
    if(length(input$variableW) != 0 & length(input$variableY)!=0){
      rho = rho()
    }else{
      rho = "Please check whether Vector of Y and Matrix of W have been generated."
    }
  })

  #Function to calculate the multiplication of Matrix of X, Kronecker Matrix Z*I and Matrix of J
  A = reactive({
    X = X()
    ZI = ZI()
    J = J()
    A = X%*%ZI%*%J
    return(A)
  })

  output$warningA = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  output$inputrowA = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0){
      return(NULL)
    }else{
      matrix = data.frame(A())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowA', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  output$inputcolA = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0){
      return(NULL)
    }else{
      matrix = data.frame(A())
      div(style="display: inline-block;vertical-align:top;", textInput('numcolA', "Number of columns to display: ",), span('from '), span(as.character(ncol(matrix))), span("columns"))
    }
  })

  output$matrixA = renderDT({
    if(length(input$variableX) != 0 & length(input$variableJ) != 0 & length(input$variableZ) != 0){
      matrix = data.frame(A())
      if(!isTruthy(input$numrowA) & !isTruthy(input$numcolA)){
        matrix = matrix[1:6,1:6]
      }else if(!isTruthy(input$numrowA)){
        matrix = matrix[1:6,1:input$numcolA]
      }else if(!isTruthy(input$numcolA)){
        matrix = matrix[1:input$numrowA,1:6]
      }else{
        matrix = matrix[1:input$numrowA,1:input$numcolA]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })

  #Fungsi untuk menghitung hasil prediksi dari Parameter Beta Nol
  # b0 = reactive({
  #   W = W()
  #   A = A()
  #   rho = rho()
  #   # rhoNew = rhoNew()
  #   # tolerance = 0.001
  #   # error = rho-rhoNew
  #   # while(error>tolerance){
  #   #   rho=rhoNew
  #   # }
  #   Y = Y()
  #   b0 = inv(t(A)%*%A)%*%t(A)%*%Y-(inv(t(A)%*%A)%*%t(A)%*%W%*%Y)%*%rho
  #   return(b0)
  # })

  output$warningb0 = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of W, matrix of X, vector of Y, or matrix of Z")
    }
  })

  output$inputrowb0 = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = rhoBeta["b0"]
      matrix = data.frame(matrix)
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowb0', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  output$b0 = renderDataTable({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      matrix = rhoBeta["b0"]
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowb0)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowb0,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)

  #Fungsi untuk menghitung hasil prediksi dari Parameter Beta
  # beta = reactive({
  #   ZI = ZI()
  #   J = J()
  #   b0 = b0()
  #   beta = ZI%*%J%*%b0
  #   return(beta)
  # })
  output$warningbeta = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of W, matrix of X, vector of Y, or matrix of Z")
    }
  })
  output$inputrowbeta = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = rhoBeta["beta"]
      matrix = data.frame(matrix)
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowBeta', "Jumlah baris yang ingin ditampilkan: "), span('dari '), span(as.character(nrow(matrix))), span("baris"))
    }
  })
  output$beta = renderDataTable({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      matrix = rhoBeta["beta"]
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowBeta)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowBeta,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)

  output$warningbetawrtX = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })
  betaWRTX = reactive({
    matrix = matrix(as.numeric(unlist(rhoBeta["beta"])), nrow = nrow(dataframe()), ncol = length(input$variableX), byrow = FALSE)
  })
  output$inputrowbetawrtX = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(betaWRTX())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowBetaWRTX', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })

  output$inputcolbetawrtX = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(betaWRTX())
      div(style="display: inline-block;vertical-align:top;", textInput('numcolBetaWRTX', "Number of columns to display: ",), span('from '), span(as.character(ncol(matrix))), span("columns"))
    }
  })

  output$betawrtX = renderDT({
    if(length(input$variableX) != 0 & length(input$variableJ) != 0 & length(input$variableZ) != 0){
      matrix = data.frame(betaWRTX())
      if(!isTruthy(input$numrowBetaWRTX) & !isTruthy(input$numcolBetaWRTX)){
        matrix = matrix[1:nrow(matrix),1:ncol(matrix)]
      }else if(!isTruthy(input$numrowBetaWRTX)){
        matrix = matrix[1:nrow(matrix),1:input$numcolBetaWRTX]
      }else if(!isTruthy(input$numcolBetaWRTX)){
        matrix = matrix[1:input$numrowBetaWRTX,1:ncol(matrix)]
      }else{
        matrix = matrix[1:input$numrowBetaWRTX,1:input$numcolBetaWRTX]
      }
      colnames(matrix) = paste(rep("[ ,", ncol(matrix)),as.character(c(1:ncol(matrix))),rep(']',ncol(matrix)))
      rownames(matrix) = paste(rep("[", nrow(matrix)),as.character(c(1:nrow(matrix))),rep(', ]',nrow(matrix)))
      matrix
    }else{
      return(NULL)
    }
  })


  #Function to calculate the prediction result of Estimated Y (Rainfall)
  YHat = reactive({
    ZI = ZI()
    J = J()

    b0 = as.numeric(unlist(rhoBeta["b0"]))
    beta = as.numeric(unlist(rhoBeta["beta"]))
    # print(b0)
    # print(beta)
    rho = rho()
    X = X()
    W = W()
    Y = Y()
    yhat = ((W%*%Y)%*%rho)+(X%*%beta)
    return(yhat)
  })

  output$warningYHat = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  output$inputrowYHat = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(YHat())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowYHat', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  output$YHat = renderDataTable({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      matrix = YHat()
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowYHat)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowYHat,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)

  output$warningPrediction = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  output$warningDownloadData = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  #Function to display download prediction data
  dataPrediction = reactive({
    #dataSelected = dataframe()[,input$variablePrediction]
    dataPrediction1 = betaWRTX()
    colnames(dataPrediction1) = paste(rep("beta", ncol(dataPrediction1)),as.character(c(1:ncol(dataPrediction1))))
    YHat = YHat()
    colnames(YHat) = "Yhat"
    matrix = cbind(dataPrediction1,YHat)
  })
  output$optionColumnPrediction = renderUI({
    dataFrame = dataframe()
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      div(id = "colPrediction", style="margin-top:30px;",
          checkboxGroupInput("variablePrediction", "Select Columns of Location, Latitude, and Longitude:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })

  output$inputrowdataPrediction = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(dataPrediction())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowdataPrediction', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })

  output$inputcoldataPrediction = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(dataPrediction())
      div(style="display: inline-block;vertical-align:top;", textInput('numcoldataPrediction', "Number of columns to display: ",), span('from '), span(as.character(ncol(matrix))), span("columns"))
    }
  })

  output$optionColumnDownloadData = renderUI({
    dataFrame = dataframe()
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0 | input$downloaddata != "AD"){
      return(NULL)
    }else{
      div(id = "colDownloadData", style="margin-top:30px;",
          checkboxGroupInput("variableDownloadData", "Select Columns of Location, Latitude, and Longitude:", inline=TRUE, colnames(dataFrame)),
      )
    }
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(paste(input$downloaddata,"-"), Sys.time(), ".csv", sep="")
    },

    content = function(file) {
      if(input$downloaddata == "VY"){
        dataFrame = Y()
      }else if(input$downloaddata == "MX"){
        dataFrame = X()
      }else if(input$downloaddata == "MW"){
        dataFrame = W()
      }else if(input$downloaddata == "MJ"){
        dataFrame = J()
      }else if(input$downloaddata == "MZ"){
        dataFrame = Z()
      }else if(input$downloaddata == "MKZ"){
        dataFrame = ZI()
      }else if(input$downloaddata == "MA"){
        dataFrame = A()
      }else if(input$downloaddata == "VB0"){
        dataFrame = as.numeric(unlist(rhoBeta["b0"]))
      }else if(input$downloaddata == "VB"){
        dataFrame = as.numeric(unlist(rhoBeta["beta"]))
      }else if(input$downloaddata == "VBX"){
        dataFrame = betaWRTX()
      }else if(input$downloaddata == "VBY"){
        dataFrame = YHat()
      }else if(input$downloaddata == "VBE"){
        dataFrame = Error()
      }else if(input$downloaddata == "AD"){
        dataFrame = dataPrediction()
        if(length(input$variableDownloadData)!=0){
          dataSelected = dataframe()[,input$variableDownloadData]
          dataFrame = cbind(dataSelected, dataFrame)
          colnamesPrediction = c(input$variableDownloadData, paste(rep("beta", ncol(betaWRTX())),as.character(c(1:ncol(betaWRTX())))), "Yhat")
          colnames(dataFrame) = colnamesPrediction
        }else{
          colnames(dataFrame) = c(paste(rep("beta", ncol(betaWRTX())),as.character(c(1:ncol(betaWRTX())))), "Yhat")
        }

      }

      write.table(dataFrame, file, sep=input$separator)
    })

  output$showDownloadData = renderDT({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      if(input$downloaddata == "VY"){
        dataFrame = Y()
      }else if(input$downloaddata == "MX"){
        dataFrame = X()
      }else if(input$downloaddata == "MW"){
        dataFrame = W()
      }else if(input$downloaddata == "MJ"){
        dataFrame = J()
      }else if(input$downloaddata == "MZ"){
        dataFrame = Z()
      }else if(input$downloaddata == "MKZ"){
        dataFrame = ZI()
      }else if(input$downloaddata == "MA"){
        dataFrame = A()
      }else if(input$downloaddata == "VB0"){
        dataFrame = as.numeric(unlist(rhoBeta["b0"]))
      }else if(input$downloaddata == "VB"){
        dataFrame = as.numeric(unlist(rhoBeta["beta"]))
      }else if(input$downloaddata == "VBX"){
        dataFrame = betaWRTX()
      }else if(input$downloaddata == "VBY"){
        dataFrame = YHat()
      }else if(input$downloaddata == "VBE"){
        dataFrame = Error()
      }else if(input$downloaddata == "AD"){
        dataFrame = dataPrediction()
      }
      matrix = data.frame(dataFrame)
      if(input$downloaddata == "AD"){
        if(length(input$variableDownloadData)!=0){
          dataSelected = dataframe()[,input$variableDownloadData]
          matrix = cbind(dataSelected, dataFrame)
          colnamesPrediction = c(input$variableDownloadData, paste(rep("beta", ncol(betaWRTX())),as.character(c(1:ncol(betaWRTX())))), "Yhat")
          colnames(matrix) = colnamesPrediction
        }else{
          colnames(matrix) = c(paste(rep("beta", ncol(betaWRTX())),as.character(c(1:ncol(betaWRTX())))), "Yhat")
        }
        matrix
      }else{
        matrix
      }

    }else{
      return(NULL)
    }
  })


  #Function to calculate the error value of Y and Estimated Y
  Error = reactive({
    Y = Y()
    Yhat = YHat()
    Error = abs(Y-Yhat)
    return(Error)
  })

  output$warningError = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  output$inputrowError = renderUI({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return(NULL)
    }else{
      matrix = data.frame(Error())
      div(style="display: inline-block;vertical-align:top;margin-top:30px;", textInput('numrowError', "Number of rows to display: "), span('from '), span(as.character(nrow(matrix))), span("rows"))
    }
  })
  output$Error = renderDataTable({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      matrix = Error()
      matrix = data.frame(matrix)
      colnames(matrix) = "[ , 1]"
      if(!isTruthy(input$numrowError)){
        matrix
      }else{
        matrix = data.frame(matrix[1:input$numrowError,])
        colnames(matrix) = "[ , 1]"
        matrix
      }
    }else{
      return(NULL)
    }
  }, server = FALSE,
  selection = list(mode = 'single',target="cell"),
  rownames= FALSE)

  #Function to calculate the RMSE value
  RMSE = reactive({
    return(sqrt(mean((Error())^2)))
  })

  output$warningRMSE = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  output$RMSE = renderText({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      RMSE = RMSE()
    }else{
      return(NULL)
    }
  })

  #Function to calculate the MAPE value
  MAPE = reactive({
    Y = Y()
    return(mean(abs((Error())/Y))*100)
  })

  output$warningMAPE = renderText({
    if(length(input$variableX) == 0 | length(input$variableJ) == 0 | length(input$variableZ) == 0 | length(input$variableY) == 0 | length(input$variableW) == 0){
      return("Please fill in matrix of X, matrix of Y, or matrix of Z")
    }
  })

  output$MAPE = renderText({
    if(length(input$variableX) != 0 | length(input$variableJ) != 0 | length(input$variableZ) != 0 | length(input$variableY) != 0 | length(input$variableW) != 0){
      MAPE = MAPE()
    }else{
      return(NULL)
    }
  })
}
