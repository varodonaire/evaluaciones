test_that("calculate_difficulty_index es la proporcion de aciertos", {
  scored <- tibble::tibble(
    Origen = c("P1", "P1", "P1", "P1", "P2", "P2"),
    is_correct = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  dif <- calculate_difficulty_index(scored)

  expect_equal(dif$dificultad[dif$Origen == "P1"], 0.75)
  expect_equal(dif$dificultad[dif$Origen == "P2"], 0)
})

test_that("calculate_difficulty_index clasifica segun los cortes documentados", {
  # Un item por tramo: 0.10 / 0.30 / 0.50 / 0.70 / 0.90
  scored <- dplyr::bind_rows(lapply(
    c(P1 = 1, P2 = 3, P3 = 5, P4 = 7, P5 = 9),
    function(k) tibble::tibble(is_correct = c(rep(TRUE, k), rep(FALSE, 10 - k)))
  ), .id = "Origen")

  dif <- calculate_difficulty_index(scored)
  expect_equal(
    dif$clasificacion[order(dif$Origen)],
    c("Muy dificil", "Dificil", "Moderado", "Facil", "Muy facil")
  )
})

test_that("calculate_difficulty_index puede desagregar por forma", {
  scored <- tibble::tibble(
    Origen = "P1", Forma = c(1L, 1L, 2L, 2L),
    is_correct = c(TRUE, TRUE, TRUE, FALSE)
  )
  dif <- calculate_difficulty_index(scored, by_form = TRUE)

  expect_equal(nrow(dif), 2)
  expect_equal(dif$dificultad[dif$Forma == 1], 1.0)
  expect_equal(dif$dificultad[dif$Forma == 2], 0.5)
})

test_that("calculate_discrimination_index detecta un item que discrimina al reves", {
  # Grupo alto: acierta P1-P3 y falla P_mala (total = 3)
  # Grupo bajo: falla P1-P3 y acierta P_mala (total = 1)
  items_buenos <- paste0("P", 1:3)
  altos <- tidyr::crossing(
    student_id = paste0("alto", 1:5),
    Origen = c(items_buenos, "P_mala")
  ) |>
    dplyr::mutate(is_correct = Origen != "P_mala")
  bajos <- tidyr::crossing(
    student_id = paste0("bajo", 1:5),
    Origen = c(items_buenos, "P_mala")
  ) |>
    dplyr::mutate(is_correct = Origen == "P_mala")
  scored <- dplyr::bind_rows(altos, bajos)

  disc <- calculate_discrimination_index(scored, method = "pbis")
  expect_gt(disc$discriminacion[disc$Origen == "P1"], 0)
  expect_lt(disc$discriminacion[disc$Origen == "P_mala"], 0)
  expect_equal(disc$clasificacion[disc$Origen == "P_mala"], "Negativa (revisar)")
})

test_that("calculate_discrimination_index soporta el metodo D27", {
  scored <- fake_scored(n_students = 30)
  disc <- calculate_discrimination_index(scored, method = "d27")

  expect_true(all(c("Origen", "discriminacion", "clasificacion") %in% names(disc)))
  expect_true(all(disc$discriminacion >= -1 & disc$discriminacion <= 1, na.rm = TRUE))
})

test_that("calculate_discrimination_index rechaza metodos desconocidos", {
  expect_error(calculate_discrimination_index(fake_scored(), method = "otro"))
})

test_that("calculate_reliability devuelve KR-20 en rango y coherente con alpha", {
  scored <- fake_scored(n_students = 40)
  rel <- calculate_reliability(scored)

  expect_equal(rel$kr20, rel$alpha)
  expect_lte(rel$kr20, 1)
  expect_equal(rel$n_students, 40)
  expect_true(rel$clasificacion %in%
    c("Inaceptable", "Pobre", "Cuestionable", "Aceptable", "Bueno", "Excelente"))
})

test_that("calculate_reliability da KR-20 alto cuando la prueba es consistente", {
  # Mitad de estudiantes acierta todo, mitad falla todo: consistencia maxima
  scored <- tidyr::crossing(
    student_id = paste0("s", 1:20),
    Origen = paste0("P", 1:10)
  ) |>
    dplyr::mutate(is_correct = as.integer(sub("s", "", student_id)) <= 10)

  rel <- calculate_reliability(scored)
  expect_gt(rel$kr20, 0.9)
  expect_equal(rel$clasificacion, "Excelente")
  # KR-20 puede pasar de 1 por poco; el SEM debe seguir siendo un numero
  expect_true(is.finite(rel$sem))
})

test_that("calculate_reliability falla claramente en casos degenerados", {
  # Un solo item con varianza
  un_item <- tibble::tibble(
    student_id = paste0("s", 1:10), Origen = "P1",
    is_correct = rep(c(TRUE, FALSE), 5)
  )
  expect_error(calculate_reliability(un_item), "al menos 2 items")

  # Todos responden identico: ningun item tiene varianza, no queda que analizar
  sin_varianza <- tidyr::crossing(
    student_id = paste0("s", 1:10),
    Origen = paste0("P", 1:5)
  ) |>
    dplyr::mutate(is_correct = Origen %in% c("P1", "P2"))
  expect_error(calculate_reliability(sin_varianza), "al menos 2 items")
})

test_that("calculate_reliability calcula el error estandar de medida", {
  rel <- calculate_reliability(fake_scored(n_students = 40))
  expect_true(is.finite(rel$sem))
  expect_gte(rel$sem, 0)
})

test_that("analyze_distractors reparte las respuestas por grupo de rendimiento", {
  scored <- fake_scored(n_students = 30)
  dist <- analyze_distractors(scored)

  expect_true(all(c("Origen", "grupo", "Respuesta", "prop", "es_correcta") %in% names(dist)))
  expect_true(all(dist$grupo %in% c("Alto", "Medio", "Bajo")))
  # Las proporciones suman 1 dentro de cada item x clave x grupo. Un mismo
  # Origen puede aparecer con distinta clave segun la forma, y esas variantes
  # se contabilizan por separado.
  sumas <- dist |>
    dplyr::group_by(Origen, Correcta, grupo) |>
    dplyr::summarise(total = sum(prop), .groups = "drop")
  expect_true(all(abs(sumas$total - 1) < 1e-8))
})

test_that("compare_forms resume cada forma y evalua equivalencia", {
  scored <- fake_scored(n_forms = 2, n_students = 40)
  cmp <- compare_forms(scored)

  expect_equal(cmp$n_forms, 2)
  expect_equal(nrow(cmp$form_stats), 2)
  expect_true(all(c("promedio", "mediana", "desv_std") %in% names(cmp$form_stats)))
  expect_true(is.logical(cmp$forms_equivalent))
})

test_that("compare_forms no intenta ANOVA con una sola forma", {
  scored <- fake_scored(n_forms = 1, n_students = 20)
  cmp <- compare_forms(scored)

  expect_equal(cmp$n_forms, 1)
  expect_true(is.na(cmp$anova_p_value))
  expect_true(cmp$forms_equivalent)
})

test_that("detect_problematic_items marca cada tipo de problema", {
  difficulty <- tibble::tibble(
    Origen = c("ok", "neg", "dificil", "facil", "baja"),
    dificultad = c(0.5, 0.5, 0.05, 0.99, 0.5),
    clasificacion = "Moderado"
  )
  discrimination <- tibble::tibble(
    Origen = c("ok", "neg", "dificil", "facil", "baja"),
    discriminacion = c(0.45, -0.2, 0.45, 0.45, 0.05),
    clasificacion = "Buena"
  )

  res <- detect_problematic_items(difficulty, discrimination)

  expect_equal(res$n_problematic, 4)
  expect_false("ok" %in% res$problematic_items$Origen)
  problemas <- stats::setNames(res$all_items$problemas, res$all_items$Origen)
  expect_equal(unname(problemas["neg"]), "Discriminacion negativa")
  expect_equal(unname(problemas["dificil"]), "Demasiado dificil")
  expect_equal(unname(problemas["facil"]), "Demasiado facil")
  expect_equal(unname(problemas["baja"]), "Baja discriminacion")
  expect_true(is.na(problemas["ok"]))
})

test_that("run_psychometric_analysis devuelve el analisis completo", {
  scored <- fake_scored(n_students = 30)
  res <- suppressMessages(run_psychometric_analysis(scored))

  expect_named(
    res,
    c("difficulty", "discrimination", "reliability", "distractors",
      "form_comparison", "problematic_items", "timestamp")
  )
  expect_s3_class(res$difficulty, "data.frame")
  expect_true(is.numeric(res$reliability$kr20))
})

test_that("run_psychometric_analysis funciona con datos del pipeline de correccion", {
  # scored_data trae "Rut", no "student_id"
  scored <- fake_scored(n_students = 30)
  expect_true("Rut" %in% names(scored))
  expect_no_error(suppressMessages(run_psychometric_analysis(scored)))
})
