if(FALSE){

  # devtools::install_github("zarquon42b/Morpho")
  # devtools::install_github("stla/SurfaceReconstruction")


  # library -----------------------------------------------------------------
  {
    source("https://raw.githubusercontent.com/statpng/pngfunction/master/tidyverse/functions.R")
    source("https://raw.githubusercontent.com/statpng/pngfunction/master/mesh3d/functions.R")

    library(tidyverse)
    library(broom)
    library(survival)
    library(broom)
    library(patchwork)

    library(RColorBrewer)
    RColorBrewer::brewer.pal(3, "Greens")[1:2]
    RColorBrewer::brewer.pal(3, "Reds")[1:2]
  }






  # data --------------------------------------------------------------------

  png.plt2stl <- function(path){
    # path <- "/Volumes/T7/1.Mesh3d/temp1/voltmap.plt/"
    ListFiles <- list.files(path, "plt", full.names = TRUE)
    FileNames <- gsub(".plt", "", map_chr(ListFiles, ~strsplit(.x, "/")[[1]] %>% {.[length(.)]}))
    MESH <- png.read_plts( ListFiles, nmax=Inf, center=TRUE )
    MESH %>% map2( FileNames, ~ Rvcg::vcgStlWrite(.x, filename=paste0(gsub(".plt","",.y)) ) )
  }



  {

    ListFiles <- list.files("/Volumes/T7/Yonsei/1. Yonsei/plt", "plt", full.names = TRUE)
    # FileNames <- stringr::str_extract(ListFiles, "\\d+.plt") %>% gsub(".plt","",.)

    MESH <- png.read_plts( ListFiles, nmax=5, center=TRUE )

    mesh <- MESH

    png.plt2stl("/Volumes/T7/Yonsei/1. Yonsei/plt")
    png.plt2stl( "/Volumes/T7/1.Mesh3d/temp1/voltmap.plt/" )


    landmark <- map(mesh, ~ .x$vb[1:3,] %>% t %>% as.data.frame %>% filter(V1<0) %>% slice_sample(prop=0.01) %>% as.matrix)

    map(mesh, ~.x$vb[1:3,] %>% t %>% apply(2,mean) %>% t) %>% do.call("rbind", .)


    #
  }



  # plot --------------------------------------------------------------------

  {
    # basic mesh3d plot
    map(mesh, png.mesh3d.plot)

    # mesh3d plot with arbitrary landmarks
    png.mesh3d.plot(mesh[[1]], landmark[[1]], radius=0.1)
  }







  # landmarking -------------------------------------------------------------

  {
    png.mesh2df(mesh[[1]]) %>% as.data.frame %>% slice_sample(prop=0.01) %>%
      png.plotly.scatter3d()


    atrial_landmark1 <-
      matrix(c(
        -1366, -302, 286,
        -1275, 280, 309,
        -1207, 218, 370,
        -1352, 330, 288,
        -1175, 219, 199,
        -1911, 243, 200,
        -1177, -28, 392
      ), ncol=3, byrow=TRUE)



    mesh[[1]] %>% png.plotly.scatter3d() %>% png.plotly.add_landmark(atrial_landmark1, size=4)

    set.seed(1)
    landmark1 <- mesh[[1]] %>% png.mesh2df %>% as.data.frame %>% slice_sample(prop=0.001) %>% as.matrix
    set.seed(1)
    landmark2 <- mesh[[2]] %>% png.mesh2df %>% as.data.frame %>% slice_sample(prop=0.001) %>% as.matrix

    png.mesh3d.plot(mesh[[1]], landmark1)
    png.mesh3d.plot(mesh[[2]], landmark2)


  }






  # ICP ---------------------------------------------------------------------

  library(Morpho)

  {
    angle=50
    fit.icp1 <- png.compare_icp(mesh.org=mesh[[1]],
                               mesh.target=mesh[[2]] %>% png.mesh3d.rotate(angle),
                               landmark.org=landmark1,
                               landmark.target=landmark2 %*% png.create_rotmat3d(angle),
                               radius = c(1,1,1)*0.1,
                               use.icp=TRUE)

    angle=100
    fit.icp2 <- png.compare_icp(mesh.org=mesh[[1]],
                                mesh.target=mesh[[2]] %>% png.mesh3d.rotate(angle),
                                landmark.org=landmark1,
                                landmark.target=landmark2 %*% png.create_rotmat3d(angle),
                                radius = c(1,1,1)*0.1,
                                use.icp=TRUE)

    angle=100
    fit.icp3 <- png.compare_icp(mesh.org=mesh[[1]] %>% png.mesh3d.rotate(40),
                                mesh.target=mesh[[2]] %>% png.mesh3d.rotate(angle),
                                landmark.org=landmark1 %*% png.create_rotmat3d(40),
                                landmark.target=landmark2 %*% png.create_rotmat3d(angle),
                                radius = c(1,1,1)*0.1,
                                use.icp=TRUE)


    angle=60
    fit.icp2 <- png.compare_icp(mesh.org=mesh[[1]],
                                mesh.target=mesh[[2]] %>% png.mesh3d.rotate(angle),
                                landmark.org=landmark1,
                                landmark.target=landmark2 %*% png.create_rotmat3d(angle),
                                radius = c(1,1,1)*0.1,
                                use.icp=TRUE)

    angle=60
    fit.icp4 <- png.compare_icp(mesh.org=mesh[[1]],
                               mesh.target=mesh[[2]] %>% png.mesh3d.rotate(angle),
                               landmark.org=landmark[[1]],
                               landmark.target=landmark[[2]] %*% png.create_rotmat3d(angle),
                               radius = c(1,1,1)*0.1,
                               use.icp=TRUE)

  }






  # Mean Mesh ---------------------------------------------------------------

  {

    n.node.min <- map_int(mesh, ~.x$vb %>% ncol) %>% min

    mean_mesh <- reduce(map(mesh, ~.x$vb[1:3,] %>% t %>% as.data.frame %>% slice_sample(n=n.node.min) %>% as.matrix), `+`) / length(mesh)

    open3d()
    mean_mesh %>% as.data.frame %>% slice_sample(prop=0.1) %>% spheres3d(radius=0.1)

    #

    #


    out <- closemeshKD(mesh[[1]], png.mesh(nose2), sign=TRUE, method=1)
    png.mesh3d(out$vb[1:3,] %>% t)


  }






  # Non-rigid Registration --------------------------------------------------

  {


    library(shapes)
    arr <- map(mesh, ~ .x %>% png.mesh2df %>% as.data.frame %>% slice_sample(n=1000) %>% as.matrix) %>% simplify2array()
    fig.GPA<-procGPA(arr)
    shapepca(fig.GPA,type="r",mag=3, project=1)


    fit <- Morpho::computeTransform(mesh[[1]] %>% png.mesh2df %>% as.data.frame %>% slice_sample(n=10000) %>% as.matrix %>% scale,
                                    mesh[[2]] %>% png.mesh2df %>% as.data.frame %>% slice_sample(n=10000) %>% as.matrix %>% scale, type="tps")


    mesh[[1]] %>% png.mesh2df %>% as.data.frame %>% slice_sample(n=10000) %>% as.matrix %>% scale %>% png.df2mesh %>% shade3d(color="grey80")
    mesh[[2]] %>% png.mesh2df %>% as.data.frame %>% slice_sample(n=10000) %>% as.matrix %>% scale %>% png.df2mesh %>% shade3d(color="grey80")

    (mesh[[1]] %>% png.mesh2df) %*% fit[1:3,] %>% png.df2mesh() %>%
    shade3d(color="grey80")


    library(Morpho)
    fit.tps <- tps3d(mesh[[1]],
                     mesh[[1]] %>% png.mesh2df %>% as.data.frame %>% slice_sample(n=10000) %>% as.matrix,
                     mesh[[2]] %>% png.mesh2df %>% as.data.frame %>% slice_sample(n=10000) %>% as.matrix, lambda=10)


    shade3d(fit.tps, color="grey80")







    icp <- Morpho::icpmat(nose1, nose2, iterations=50)

    nose1 %>% png.mesh3d()
    nose2 %>% png.mesh3d()
    out <- closemeshKD(nose1, png.mesh(nose2), sign=TRUE, method=1)
    png.mesh3d(out$vb[1:3,] %>% t)
  }








  # Atlas -------------------------------------------------------------------

  library(rgl)
  {
    atlas <- createAtlas(mesh[[1]],
                         landmarks=landmark[[1]],
                         patch=landmark[[1]])
    plotAtlas(atlas)

    tps <- tps3d(mesh[[2]], landmark[[2]], landmark[[1]][-1,])


    deformGrid3d(mesh[[2]], tps, ngrid = 5)



    {
      data(nose)
      ## define some landmarks
      refind <- c(1:3,4,19:20)
      ## use a subset of shortnose.lm as anchor points for a TPS-deformation
      reflm <- shortnose.lm[refind,]
      tarlm <- reflm
      ##replace the landmark at the tip of the nose with that of longnose.lm
      tarlm[4,] <- longnose.lm[4,]
      ##  deform a set of semilandmarks by applying a TPS-deformation
      ##  based on 5 reference points
      deformed <- tps3d(shortnose.lm, reflm, tarlm,threads=1)
      ## Not run:
      ##visualize results by applying a deformation grid
      deformGrid3d(shortnose.lm,deformed,ngrid = 5)


      data(nose)##load data
      ##warp a mesh onto another landmark configuration:
      longnose.mesh <- tps3d(shortnose.mesh,shortnose.lm,longnose.lm,threads=1)


      require(rgl)
      shade3d(longnose.mesh,col=skin1)

      ## End(Not run)

      data(boneData)
      ## deform mesh belonging to the first specimen
      ## onto the landmark configuration of the 10th specimen

      ## Not run:
      warpskull <- tps3d(skull_0144_ch_fe.mesh,boneLM[,,1],
                         boneLM[,,10], threads=1)
      ## render deformed mesh and landmarks
      shade3d(warpskull, col=2, specular=1)
      spheres3d(boneLM[,,1])
      ## render original mesh
      shade3d(skull_0144_ch_fe.mesh, col=3, specular=1)
      spheres3d(boneLM[,,10])

    }

  }




  # Lithics3D ---------------------------------------------------------------

  # devtools::install_github("cornelmpop/Lithics3D")
  library(Lithics3D)


  Lithics3D::alignMesh.PCA()

  alignedMesh<-alignMesh.PCA(demoFlake1$mesh)

  Morpho::pcAlign()




  #






  # 2 -----------------------------------------------------------------------

  ## Not run:
  data(nose)
  require(rgl)
  ###create mesh for longnose
  longnose.mesh <- tps3d(shortnose.mesh,shortnose.lm,longnose.lm,threads=1)
  ## create atlas
  fix <- c(1:5,20:21)
  atlas <- createAtlas(shortnose.mesh, landmarks =
                         shortnose.lm[fix,], patch=shortnose.lm[-c(1:5,20:21),])
  ## view atlas

  plotAtlas(atlas)

  ## create landmark array with only fix landmarks
  data <- bindArr(shortnose.lm[fix,], longnose.lm[fix,], along=3)
  dimnames(data)[[3]] <- c("shortnose", "longnose")

  ### write meshes to disk
  mesh2ply(shortnose.mesh, filename="shortnose")
  mesh2ply(longnose.mesh, filename="longnose")

  patched <- placePatch(atlas, data, path="./", inflate=5)
  ## now browse through placed patches
  checkLM(patched, path="./", atlas=atlas)

  ## same example with only one target specimen
  data <- longnose.lm[fix, ]

  patched <- placePatch(atlas, data, prefix="longnose", path="./", inflate=5)
  wire3d(longnose.mesh,col=3)
  spheres3d(patched)










  # projection --------------------------------------------------------------

  data(nose)
  out <- closemeshKD(longnose.lm, shortnose.mesh, sign=TRUE, method=1)
  out2 <- closemeshKD(shortnose.lm, longnose.mesh, sign=TRUE, method=1)
  ### show distances - they are very small because
  ###longnose.lm is scaled to unit centroid size.
  hist(out$quality)
  hist(out2$quality)


  png.mesh3d(out$vb[1:3,] %>% t)
  png.mesh3d(out2$vb[1:3,] %>% t)
  out2












}
