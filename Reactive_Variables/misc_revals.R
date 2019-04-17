# keep track of dimension of peakData2....
peakData2_dim <- eventReactive(revals$peakData2, {
  prod(dim(revals$peakData2$e_data[,-1]))
})
#... and dimension of uploaded_data
uploaded_data_dim <- eventReactive(uploaded_data(),{
  prod(dim(uploaded_data()$e_data[,-1]))
})

peakData_dim <- eventReactive(peakData(), {
  prod(dim(peakData()$e_data[,-1]))
})